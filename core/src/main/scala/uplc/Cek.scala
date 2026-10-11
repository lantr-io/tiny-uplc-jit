package uplc

import scala.annotation.tailrec

/** Values produced by the CEK machine. Closures capture their environment explicitly. */
enum Value:
    case VCon(value: Const)
    case VLam(name: String, body: Term, env: Cek.Env)

    /** A builtin waiting for more arguments; `args` are kept in application order. */
    case VBuiltin(fn: DefaultFun, args: List[Value])

/** The CEK machine (Felleisen & Friedman), the reference evaluator of Plutus.
  *
  *   - **C**ontrol: the term being evaluated
  *   - **E**nvironment: values of the free variables of that term
  *   - **K**ontinuation: what to do with the result — an explicit stack of frames
  *
  * The continuation lives on the heap, so evaluation depth is bounded by memory, not by the JVM
  * thread stack: the machine is stack-safe by construction.
  *
  * Simplifications vs. the production Plutus CEK: no cost accounting (budget), and variables are
  * looked up by name in a linked list instead of by de Bruijn index.
  */
object Cek:
    import Term.*
    import Value.*

    /** Environment: innermost binding first. Lookup walks the list; lexical scoping guarantees the
      * first match is the right one.
      */
    type Env = List[(String, Value)]

    /** One continuation frame. */
    enum Frame:
        /** `[_ arg]`: computing the function; the argument is still to be computed in `env`. */
        case ApplyArg(arg: Term, env: Env)

        /** `[fun _]`: computing the argument; `fun` is the already computed function. */
        case ApplyTo(fun: Value)

        /** `(case _ branches)`: computing the scrutinee; one branch will run in `env`. */
        case CaseFrame(branches: List[Term], env: Env)

    import Frame.*

    /** Machine state: either computing a term, or returning a value to the top frame. */
    private enum State:
        case Compute(term: Term, env: Env)
        case Return(value: Value)

    import State.*

    /** Evaluates a closed term to a value, or throws [[EvaluationError]]. */
    def evaluate(term: Term): Value = run(Compute(term, Nil), Nil)

    /** The transition function. Each case is one rule of the CEK machine; `@tailrec` turns the
      * whole thing into a loop, so the JVM stack never grows.
      */
    @tailrec
    private def run(state: State, stack: List[Frame]): Value = state match
        // ---- compute: decompose the term ----
        case Compute(Var(x), env)       => run(Return(lookup(env, x)), stack)
        case Compute(LamAbs(x, b), env) => run(Return(VLam(x, b, env)), stack)
        case Compute(Constant(c), _)    => run(Return(VCon(c)), stack)
        case Compute(Builtin(fn), _)    => run(Return(VBuiltin(fn, Nil)), stack)
        case Compute(Apply(f, a), env)  => run(Compute(f, env), ApplyArg(a, env) :: stack)
        case Compute(Case(s, bs), env)  => run(Compute(s, env), CaseFrame(bs, env) :: stack)
        case Compute(Error, _)          => throw EvaluationError("(error) evaluated")

        // ---- return: feed the value to the top frame ----
        case Return(v) =>
            stack match
                case Nil                      => v
                case ApplyArg(arg, env) :: k  => run(Compute(arg, env), ApplyTo(v) :: k)
                case ApplyTo(fun) :: k        => run(applyValue(fun, v), k)
                case CaseFrame(bs, env) :: k  => run(Compute(bs(branchIndex(v, bs.size)), env), k)

    /** Function application: enter a lambda body, or feed one more argument to a builtin. */
    private def applyValue(fun: Value, arg: Value): State = fun match
        case VLam(x, body, env) => Compute(body, (x, arg) :: env)
        case VBuiltin(fn, args) =>
            val newArgs = args :+ arg // at most 2 arguments: appending is fine
            if newArgs.size == fn.arity then Return(applyBuiltin(fn, newArgs))
            else Return(VBuiltin(fn, newArgs))
        case other => throw EvaluationError(s"cannot apply ${describe(other)}")

    /** Selects a `case` branch: only builtin Bool scrutinees are supported. */
    private def branchIndex(scrutinee: Value, branchCount: Int): Int = scrutinee match
        case VCon(Const.Bool(b)) => BoolCase.branchIndex(b, branchCount)
        case other => throw EvaluationError(s"case: expected bool, got ${describe(other)}")

    /** Runs a saturated builtin. Arguments are checked here, when the builtin is saturated. */
    private def applyBuiltin(fn: DefaultFun, args: List[Value]): Value =
        import DefaultFun.*
        def int(v: Value): BigInt = v match
            case VCon(Const.Integer(i)) => i
            case other => throw EvaluationError(s"$fn: expected integer, got ${describe(other)}")
        def integer(i: BigInt): Value = VCon(Const.Integer(i))
        def boolean(b: Boolean): Value = VCon(Const.Bool(b))

        (fn, args) match
            case (AddInteger, List(a, b))            => integer(int(a) + int(b))
            case (SubtractInteger, List(a, b))       => integer(int(a) - int(b))
            case (MultiplyInteger, List(a, b))       => integer(int(a) * int(b))
            case (EqualsInteger, List(a, b))         => boolean(int(a) == int(b))
            case (LessThanEqualsInteger, List(a, b)) => boolean(int(a) <= int(b))
            case _ => throw IllegalStateException(s"$fn applied to ${args.size} arguments")

    private def lookup(env: Env, name: String): Value =
        // Hand-written loop: this is the hottest path of the interpreter.
        @tailrec def go(e: Env): Value = e match
            case (n, v) :: rest => if n == name then v else go(rest)
            case Nil            => throw EvaluationError(s"free variable: $name")
        go(env)

    private def describe(v: Value): String = v match
        case VCon(c)         => c.show
        case _: VLam         => "a lambda"
        case VBuiltin(fn, _) => s"builtin ${fn.uplcName}"

    /** The result as a constant, if it is one — used to compare engines. */
    def toConst(v: Value): Option[Const] = v match
        case VCon(c) => Some(c)
        case _       => None
