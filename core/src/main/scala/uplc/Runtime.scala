package uplc

/** Runtime value representation shared by the closure compiler and the staged JIT.
  *
  * Instead of a `Value` ADT we use plain JVM objects, so compiled code needs no wrapping:
  *
  * | UPLC value | JVM representation     |
  * |:-----------|:-----------------------|
  * | integer    | `scala.math.BigInt`    |
  * | bool       | `java.lang.Boolean`    |
  * | lambda     | `Any => Any`           |
  * | builtin    | curried `Any => Any`   |
  *
  * UPLC is untyped, so everything is `Any` and every use site checks the type at runtime, raising
  * [[EvaluationError]] exactly where the CEK machine would.
  */
object Runtime:

    /** Applies a runtime value to an argument, failing if it is not a function. */
    def apply(fun: Any, arg: Any): Any = fun match
        case f: Function1[?, ?] => f.asInstanceOf[Any => Any](arg)
        case other              => throw EvaluationError(s"cannot apply ${describe(other)}")

    def asInteger(value: Any): BigInt = value match
        case i: BigInt => i
        case other     => throw EvaluationError(s"expected integer, got ${describe(other)}")

    def asBool(value: Any): Boolean = value match
        case b: Boolean => b
        case other      => throw EvaluationError(s"case: expected bool, got ${describe(other)}")

    /** Branch selected by a `case` scrutinee — same rules as the CEK machine. */
    def caseIndex(scrutinee: Any, branchCount: Int): Int =
        BoolCase.branchIndex(asBool(scrutinee), branchCount)

    def fail(): Nothing = throw EvaluationError("(error) evaluated")

    def constant(c: Const): Any = c match
        case Const.Integer(i) => i
        case Const.Bool(b)    => b

    /** Curried builtin as a first-class value, for partial applications like `(builtin addInteger)`
      * passed as an argument. Saturated calls never go through here: compilers inline them.
      */
    def builtin(fn: DefaultFun): Any = builtinValues(fn.ordinal)

    private val builtinValues: IArray[Any] = IArray.from(DefaultFun.values.map(curried))

    private def curried(fn: DefaultFun): Any =
        import DefaultFun.*
        // The function only checks argument types once saturated, like the CEK machine.
        def binary(op: (Any, Any) => Any): Any = (a: Any) => (b: Any) => op(a, b)
        fn match
            case AddInteger            => binary((a, b) => asInteger(a) + asInteger(b))
            case SubtractInteger       => binary((a, b) => asInteger(a) - asInteger(b))
            case MultiplyInteger       => binary((a, b) => asInteger(a) * asInteger(b))
            case EqualsInteger         => binary((a, b) => asInteger(a) == asInteger(b))
            case LessThanEqualsInteger => binary((a, b) => asInteger(a) <= asInteger(b))

    /** The result as a constant, if it is one — used to compare engines. */
    def toConst(value: Any): Option[Const] = value match
        case i: BigInt  => Some(Const.Integer(i))
        case b: Boolean => Some(Const.Bool(b))
        case _          => None

    private def describe(value: Any): String = value match
        case i: BigInt          => s"(con integer $i)"
        case b: Boolean         => s"(con bool $b)"
        case _: Function1[?, ?] => "a function"
        case other              => other.toString
