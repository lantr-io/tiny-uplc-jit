package uplc

import scala.quoted.*

/** Tier 2: compile a term to Scala code at runtime with multi-stage programming, then let the
  * Scala compiler emit JVM bytecode and the JVM JIT it to machine code.
  *
  * Scala 3 staging in two operators:
  *   - a quote `'{ e }` is the *code* of `e`, an `Expr[T]`, not its value;
  *   - a splice `${ x }` inserts an `Expr` built by our generator into a quote.
  *
  * The generator below walks the UPLC term once and produces one big Scala expression where
  *   - UPLC lambdas are Scala lambdas, and UPLC variables are their JVM parameters
  *     (no environment at all: closures capture variables natively);
  *   - saturated builtins are inlined arithmetic on `BigInt`;
  *   - `case` on a Bool is a native `if`.
  *
  * `staging.run` then type-checks and compiles that expression into bytecode, loads it and returns
  * the resulting function. Quotes are type-checked when *this* file is compiled, so the generator
  * can only produce well-typed Scala.
  */
object StagedJit:
    import Term.*
    import DefaultFun.*

    /** One embedded Scala compiler instance, reused by all compilations. Creating it is cheap; the
      * first `run` pays for loading the compiler's classes (about half a second on a cold JVM).
      */
    private given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    /** Compiles a closed term to a native JVM function that evaluates it. */
    def compile(term: Term): () => Any = staging.run(program(term))

    /** The Scala source the JIT generates for `term` — what the audience sees in the demo. */
    def showCode(term: Term): String = staging.withQuotes(program(term).show)

    /** The whole program is a `() => Any`, so it can be compiled once and run many times. */
    private def program(term: Term)(using Quotes): Expr[() => Any] =
        '{ () => ${ gen(term, Map.empty) } }

    /** Generates code for `term`.
      *
      * @param env
      *   maps each UPLC variable in scope to the Scala expression holding its value — always a
      *   reference to a parameter of an enclosing generated lambda.
      */
    private def gen(term: Term, env: Map[String, Expr[Any]])(using Quotes): Expr[Any] =
        term match
            case Var(name) =>
                env.getOrElse(name, throw EvaluationError(s"free variable: $name"))

            case LamAbs(name, body) =>
                // `'arg` is the code referring to the lambda parameter: the body sees UPLC
                // variable `name` as this JVM local. Each quote creates a fresh symbol, so
                // shadowed names (`λx. λx. x`) are hygienic for free.
                '{ (arg: Any) => ${ gen(body, env.updated(name, 'arg)) } }

            case SaturatedBuiltin(fn, args) =>
                genBuiltin(fn, args, env)

            case Apply(fun, arg) =>
                // Function first, then argument: the CEK evaluation order.
                '{
                    val f = ${ gen(fun, env) }
                    Runtime.apply(f, ${ gen(arg, env) })
                }

            // `(case c else then)` becomes a native `if`: only the chosen branch runs.
            case IfThenElse(c, t, e) =>
                '{ if ${ genBool(c, env) } then ${ gen(t, env) } else ${ gen(e, env) } }

            case Case(scrutinee, branches) =>
                // Any other shape (1 branch, or an invalid count): keep the generic semantics.
                val count = Expr(branches.size)
                val selected = '{ Runtime.caseIndex(${ gen(scrutinee, env) }, $count) }
                branches match
                    case List(only) => '{ $selected; ${ gen(only, env) } } // index is 0 or failed
                    case _          => '{ $selected; Runtime.fail() } // caseIndex always fails

            case Constant(c) => genConstant(c)

            case Builtin(fn) =>
                // Unsaturated builtin used as a value: fall back to its curried form.
                '{ Runtime.builtin(DefaultFun.fromOrdinal(${ Expr(fn.ordinal) })) }

            case Error => '{ Runtime.fail() }

    /** Generates a condition as a primitive `Boolean`. When it is a comparison builtin, the result
      * is used directly: no boxing and no type check on the hot path of every recursive function.
      */
    private def genBool(cond: Term, env: Map[String, Expr[Any]])(using Quotes): Expr[Boolean] =
        cond match
            case SaturatedBuiltin(fn @ (EqualsInteger | LessThanEqualsInteger), args) =>
                genComparison(fn, args, env)
            case _ => '{ Runtime.asBool(${ gen(cond, env) }) }

    /** Inlines a saturated builtin.
      *
      * All arguments are bound to locals first: UPLC evaluates every argument before the builtin
      * inspects any of them. Writing `asInteger($a) + asInteger($b)` would type-check `a` before
      * evaluating `b` — observably different if `b` loops forever. The JVM removes these locals.
      *
      * Note `${ gen(a, env) }` *inside* the quote, not `val code = gen(a, env)` before it: an
      * `Expr` built outside the splice where it ends up has the wrong owner, so splicing it under
      * `val x = …` copies the whole subtree to fix the owner. On deeply nested terms that copy
      * repeats at every level and generation becomes quadratic in the nesting depth.
      */
    private def genBuiltin(fn: DefaultFun, args: List[Term], env: Map[String, Expr[Any]])(using
        Quotes
    ): Expr[Any] = (fn, args) match
        case (AddInteger, List(a, b)) =>
            '{
                val x = ${ gen(a, env) }; val y = ${ gen(b, env) }
                Runtime.asInteger(x) + Runtime.asInteger(y)
            }
        case (SubtractInteger, List(a, b)) =>
            '{
                val x = ${ gen(a, env) }; val y = ${ gen(b, env) }
                Runtime.asInteger(x) - Runtime.asInteger(y)
            }
        case (MultiplyInteger, List(a, b)) =>
            '{
                val x = ${ gen(a, env) }; val y = ${ gen(b, env) }
                Runtime.asInteger(x) * Runtime.asInteger(y)
            }
        case (EqualsInteger | LessThanEqualsInteger, _) =>
            genComparison(fn, args, env)
        case _ => throw IllegalStateException(s"$fn applied to ${args.size} arguments")

    private def genComparison(fn: DefaultFun, args: List[Term], env: Map[String, Expr[Any]])(using
        Quotes
    ): Expr[Boolean] = (fn, args) match
        case (EqualsInteger, List(a, b)) =>
            '{
                val x = ${ gen(a, env) }; val y = ${ gen(b, env) }
                Runtime.asInteger(x) == Runtime.asInteger(y)
            }
        case (LessThanEqualsInteger, List(a, b)) =>
            '{
                val x = ${ gen(a, env) }; val y = ${ gen(b, env) }
                Runtime.asInteger(x) <= Runtime.asInteger(y)
            }
        case _ => throw IllegalStateException(s"$fn is not a comparison")

    /** Constants become literals in the generated code. Integers that fit in a `Long` use
      * `BigInt(long)`, which returns cached instances for small values (-1024..1024); larger ones
      * are parsed from their decimal string.
      */
    private def genConstant(c: Const)(using Quotes): Expr[Any] = c match
        case Const.Integer(i) if i.isValidLong => '{ BigInt(${ Expr(i.toLong) }) }
        case Const.Integer(i)                  => '{ BigInt(${ Expr(i.toString) }) }
        case Const.Bool(b)                     => Expr(b)
