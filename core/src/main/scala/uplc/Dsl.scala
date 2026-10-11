package uplc

/** A tiny DSL so example programs look like lambda calculus rather than nested constructors.
  *
  * {{{
  * lam("x", "y")(add(v("x"), v("y")))   //  λx. λy. x + y
  * v("f")(v("x"), v("y"))                //  f x y  ==  [f x y]
  * }}}
  */
object Dsl:
    import Term.*
    import DefaultFun.*

    def v(name: String): Term = Var(name)

    /** Curried lambda: `lam("a", "b")(body)` is `λa. λb. body`. */
    def lam(names: String*)(body: Term): Term = names.foldRight(body)(LamAbs(_, _))

    def int(n: BigInt): Term = Constant(Const.Integer(n))
    def bool(b: Boolean): Term = Constant(Const.Bool(b))
    def builtin(fn: DefaultFun): Term = Builtin(fn)
    val error: Term = Error

    /** Left-associated application: `f(a, b)` is `[[f a] b]`. */
    extension (f: Term) def apply(args: Term*): Term = args.foldLeft(f)(Apply(_, _))

    def add(a: Term, b: Term): Term = builtin(AddInteger)(a, b)
    def sub(a: Term, b: Term): Term = builtin(SubtractInteger)(a, b)
    def mul(a: Term, b: Term): Term = builtin(MultiplyInteger)(a, b)
    def eqInt(a: Term, b: Term): Term = builtin(EqualsInteger)(a, b)
    def leq(a: Term, b: Term): Term = builtin(LessThanEqualsInteger)(a, b)

    /** The Plutus V3 conditional: `(case cond else then)` — `False` is branch 0. */
    def ifThen(cond: Term, thenT: Term, elseT: Term): Term = Case(cond, List(elseT, thenT))

    /** Recursion by self-application, the way Scalus compiles it:
      * {{{
      * [(lam f [f f]) (lam f body)]      // inside body, recursive calls are [f f arg]
      * }}}
      * UPLC has no `letrec`: the function receives itself as its first argument.
      */
    def recursive(name: String)(body: Term): Term =
        lam(name)(v(name)(v(name)))(lam(name)(body))

    /** A recursive call `[f f args...]` inside [[recursive]]. */
    def self(name: String, args: Term*): Term = v(name)((v(name) +: args)*)
