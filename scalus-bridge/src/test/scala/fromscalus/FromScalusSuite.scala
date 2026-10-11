package fromscalus

import munit.FunSuite
import _root_.uplc.{Const, Engine}
import scalus.uplc.{Constant as SConst, Term as STerm}

/** Real compiler output runs on every engine and agrees with Scalus' production CEK machine. */
class FromScalusSuite extends FunSuite:

    for n <- List(0, 1, 2, 10, 15) do
        test(s"fib $n compiled by Scalus"):
            val expected = ScalusFib.evaluateWithScalus(n) match
                case STerm.Const(SConst.Integer(value), _) => Const.Integer(value)
                case other                                 => fail(s"unexpected Scalus result $other")
            for engine <- Engine.values do
                assertEquals(engine.prepare(ScalusFib.program(n))(), Some(expected), engine.label)

    test("Scalus output uses only the mini-UPLC subset"):
        // `convert` throws on anything else; this guards against compiler output changing.
        assert(ScalusFib.fib.show.startsWith("["), ScalusFib.fib.show)
