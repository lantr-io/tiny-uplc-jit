package uplc

/** Tier 1: compile a term into a tree of Scala closures, no metaprogramming.
  *
  * Every term becomes a `Code = Frame => Any` function, built once. Compared to the CEK machine:
  *   - no per-step dispatch on the term shape: that work happens once, at compile time;
  *   - variable names are resolved to a fixed depth in the environment at compile time;
  *   - UPLC lambdas become JVM lambdas, and calls use the JVM call stack.
  *
  * What remains slow: every node is still a megamorphic `Function1.apply` call, and variables
  * still walk a linked list at runtime. Staging (see [[StagedJit]]) removes both.
  *
  * Trade-off: recursion in the program becomes recursion on the JVM stack, so deep (non-tail)
  * recursion can throw `StackOverflowError`, which the CEK machine never does.
  */
object ClosureCompiler:
    import Term.*
    import DefaultFun.*

    /** Runtime environment: a linked list of values, innermost first. Names are gone. */
    final class Frame(val value: Any, val parent: Frame | Null)

    /** Compiled code: given the environment of its free variables, produce a value. */
    type Code = Frame | Null => Any

    /** Compiles a closed term; the result evaluates the term each time it is called. */
    def compile(term: Term): () => Any =
        val code = compileTerm(term, Nil)
        () => code(null)

    /** @param scope
      *   variable names bound around `term`, innermost first; a variable's position in this list
      *   is its depth in the runtime [[Frame]] chain.
      */
    private def compileTerm(term: Term, scope: List[String]): Code = term match
        case Var(name) =>
            scope.indexOf(name) match
                case -1 => throw EvaluationError(s"free variable: $name")
                // Specialize the common shallow depths: no loop at runtime.
                case 0 => env => env.nn.value
                case 1 => env => env.nn.parent.nn.value
                case depth =>
                    env =>
                        var frame = env.nn
                        var i = depth
                        while i > 0 do
                            frame = frame.parent.nn
                            i -= 1
                        frame.value

        case LamAbs(name, body) =>
            val bodyCode = compileTerm(body, name :: scope)
            env => (arg: Any) => bodyCode(Frame(arg, env))

        // Saturated builtin calls are evaluated directly, without currying.
        case SaturatedBuiltin(fn, args) => compileBuiltin(fn, args.map(compileTerm(_, scope)))

        case Apply(fun, arg) =>
            val funCode = compileTerm(fun, scope)
            val argCode = compileTerm(arg, scope)
            // UPLC evaluates the function before the argument: keep that order.
            env =>
                val f = funCode(env)
                Runtime.apply(f, argCode(env))

        // The common two-branch case is a plain `if`; only the chosen branch runs.
        case IfThenElse(cond, thenT, elseT) =>
            val c = compileTerm(cond, scope)
            val t = compileTerm(thenT, scope)
            val e = compileTerm(elseT, scope)
            env => if Runtime.asBool(c(env)) then t(env) else e(env)

        case Case(scrutinee, branches) =>
            val s = compileTerm(scrutinee, scope)
            val bs = branches.map(compileTerm(_, scope)).toArray
            env => bs(Runtime.caseIndex(s(env), bs.length))(env)

        case Constant(c) =>
            val value = Runtime.constant(c) // allocated once, shared by all evaluations
            _ => value

        case Builtin(fn) =>
            val value = Runtime.builtin(fn)
            _ => value

        case Error => _ => Runtime.fail()

    /** All arguments are evaluated left to right before any type check, exactly like the CEK
      * machine, which only inspects arguments once the builtin is saturated.
      */
    private def compileBuiltin(fn: DefaultFun, args: List[Code]): Code = (fn, args) match
        case (AddInteger, List(a, b)) =>
            env => { val x = a(env); val y = b(env); Runtime.asInteger(x) + Runtime.asInteger(y) }
        case (SubtractInteger, List(a, b)) =>
            env => { val x = a(env); val y = b(env); Runtime.asInteger(x) - Runtime.asInteger(y) }
        case (MultiplyInteger, List(a, b)) =>
            env => { val x = a(env); val y = b(env); Runtime.asInteger(x) * Runtime.asInteger(y) }
        case (EqualsInteger, List(a, b)) =>
            env => { val x = a(env); val y = b(env); Runtime.asInteger(x) == Runtime.asInteger(y) }
        case (LessThanEqualsInteger, List(a, b)) =>
            env => { val x = a(env); val y = b(env); Runtime.asInteger(x) <= Runtime.asInteger(y) }
        case _ => throw IllegalStateException(s"$fn applied to ${args.size} arguments")
