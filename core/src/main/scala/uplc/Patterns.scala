package uplc

import Term.*

/** Matches a builtin applied to exactly as many arguments as its arity: `[(builtin f) a b]`.
  *
  * Over-applied builtins (`[(builtin addInteger) a b c]`) do not match at the top, but their
  * function part does, so compilers handle them through the generic `Apply` case.
  */
object SaturatedBuiltin:
    def unapply(term: Term): Option[(DefaultFun, List[Term])] =
        // Walk down the application spine, collecting arguments in order.
        def spine(t: Term, args: List[Term]): Option[(DefaultFun, List[Term])] = t match
            case Apply(fun, arg)                      => spine(fun, arg :: args)
            case Builtin(fn) if args.size == fn.arity => Some((fn, args))
            case _                                    => None
        term match
            case Apply(_, _) => spine(term, Nil)
            case _           => None

/** Matches the two-branch conditional `(case cond else then)`, yielding `(cond, then, else)`. */
object IfThenElse:
    def unapply(term: Term): Option[(Term, Term, Term)] = term match
        case Case(cond, List(elseT, thenT)) => Some((cond, thenT, elseT))
        case _                              => None
