package fromscalus

import scalus.uplc.{Constant as SConst, DefaultFun as SFun, Term as STerm}
import _root_.uplc.{Const, DefaultFun, Term}

/** Converts real Scalus UPLC into our mini-UPLC, failing loudly on anything outside the subset.
  *
  * This is how the talk shows that the subset is not a toy language: it is exactly what real
  * compiler output needs.
  */
object FromScalus:

    /** Thrown for constructs, constants or builtins our mini-UPLC does not support. */
    final class Unsupported(what: String)
        extends RuntimeException(s"not in mini-UPLC: $what")

    def convert(term: STerm): Term = term match
        // Scalus names are unique (`n-5634`), plus a de Bruijn index we do not need.
        case STerm.Var(name, _)          => Term.Var(name.name)
        case STerm.LamAbs(name, body, _) => Term.LamAbs(name, convert(body))
        case STerm.Apply(fun, arg, _)    => Term.Apply(convert(fun), convert(arg))
        case STerm.Const(c, _)           => Term.Constant(convertConst(c))
        case STerm.Builtin(fn, _)        => Term.Builtin(convertFun(fn))
        case STerm.Case(arg, cases, _)   => Term.Case(convert(arg), cases.map(convert))
        case STerm.Error(_)              => Term.Error
        case other                       => throw Unsupported(other.getClass.getSimpleName)

    private def convertConst(c: SConst): Const = c match
        case SConst.Integer(value) => Const.Integer(value)
        case SConst.Bool(value)    => Const.Bool(value)
        case other                 => throw Unsupported(s"constant $other")

    /** Builtins share their names in both enums. */
    private def convertFun(fn: SFun): DefaultFun =
        DefaultFun.values.find(_.toString == fn.toString).getOrElse(throw Unsupported(s"builtin $fn"))
