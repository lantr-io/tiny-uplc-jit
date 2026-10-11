package uplc

/** Builtin functions of our mini-UPLC: just what `fib` compiled by Scalus needs, plus two extras.
  *
  * Real UPLC has ~100 builtins (crypto, bytestrings, Data, BLS12-381...). All of them follow the
  * same pattern: curried, strict, run once saturated, type-checked at runtime.
  *
  * @param arity
  *   number of arguments after which the builtin is saturated and runs
  * @param uplcName
  *   name in UPLC concrete syntax, used by the pretty printer
  */
enum DefaultFun(val arity: Int, val uplcName: String):
    case AddInteger extends DefaultFun(2, "addInteger")
    case SubtractInteger extends DefaultFun(2, "subtractInteger")
    case MultiplyInteger extends DefaultFun(2, "multiplyInteger")
    case EqualsInteger extends DefaultFun(2, "equalsInteger")
    case LessThanEqualsInteger extends DefaultFun(2, "lessThanEqualsInteger")

/** Constants. UPLC integers are arbitrary precision, so we use `BigInt`: silently overflowing a
  * `Long` would change the meaning of a smart contract.
  */
enum Const:
    case Integer(value: BigInt)
    case Bool(value: Boolean)

    /** UPLC concrete syntax, e.g. `(con integer 42)`. */
    def show: String = this match
        case Integer(v) => s"(con integer $v)"
        case Bool(v)    => s"(con bool ${if v then "True" else "False"})"

/** The smallest subset of Untyped Plutus Core that runs real compiler output — `fib` compiled from
  * Scala by Scalus uses exactly these constructs:
  *
  *   - the untyped lambda calculus: `Var`, `LamAbs`, `Apply`;
  *   - `Constant`s and `Builtin` functions;
  *   - `Case` on a builtin `Bool`: the Plutus V3 conditional, lazy in its branches;
  *   - `Error`, the only way to fail.
  *
  * Missing vs. real Plutus V3: `Delay`/`Force`, `Constr` and `Case` on constructors, other constant
  * types, and de Bruijn indices — we keep variable names for readability.
  */
enum Term:
    case Var(name: String)
    case LamAbs(name: String, body: Term)
    case Apply(fun: Term, arg: Term)
    case Constant(value: Const)
    case Builtin(fn: DefaultFun)

    /** `(case scrutinee branch0 branch1)`: on a Bool, `False` selects branch 0 and `True` branch 1.
      * Only the selected branch is evaluated.
      */
    case Case(scrutinee: Term, branches: List[Term])
    case Error

    /** Pretty-prints in UPLC concrete syntax; nested applications are flattened: `[f a b]`. */
    def show: String = this match
        case Var(name)          => name
        case LamAbs(name, body) => s"(lam $name ${body.show})"
        case app: Apply =>
            // Collect the application spine so `[[f a] b]` prints as `[f a b]`.
            def spine(t: Term, args: List[Term]): (Term, List[Term]) = t match
                case Apply(f, a) => spine(f, a :: args)
                case other       => (other, args)
            val (head, args) = spine(app, Nil)
            (head :: args).map(_.show).mkString("[", " ", "]")
        case Constant(c)              => c.show
        case Builtin(fn)              => s"(builtin ${fn.uplcName})"
        case Case(scrutinee, branches) => (scrutinee :: branches).map(_.show).mkString("(case ", " ", ")")
        case Error                    => "(error)"

/** The only failure mode of UPLC: evaluation either returns a value or fails. Every engine throws
  * this exception, so tests can check that all of them fail on exactly the same programs.
  */
final class EvaluationError(message: String) extends RuntimeException(message)

/** Semantics of `case` on a builtin Bool, shared by all engines so they cannot disagree. */
object BoolCase:
    /** Index of the branch selected by `value` among `branchCount` branches.
      *
      * Plutus V3 rules: 1 or 2 branches; `False` selects branch 0, `True` branch 1. With a single
      * branch, `True` has nothing to select and evaluation fails.
      */
    def branchIndex(value: Boolean, branchCount: Int): Int =
        if branchCount < 1 || branchCount > 2 then
            throw EvaluationError(s"case on bool needs 1 or 2 branches, got $branchCount")
        val index = if value then 1 else 0
        if index >= branchCount then throw EvaluationError("case: no branch for True")
        index
