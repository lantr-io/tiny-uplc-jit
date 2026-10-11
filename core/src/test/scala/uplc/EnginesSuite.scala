package uplc

import munit.FunSuite

/** Every engine must agree with the CEK machine — the reference semantics — on every program,
  * including on which programs fail.
  */
class EnginesSuite extends FunSuite:
    import Dsl.*
    import DefaultFun.*
    import Term.Case

    private def evalAll(term: Term): Map[Engine, Either[String, Option[Const]]] =
        Engine.values.map { engine =>
            val outcome =
                try Right(engine.prepare(term)())
                catch case e: EvaluationError => Left(e.getMessage)
            engine -> outcome
        }.toMap

    /** Asserts that all engines return `expected`. */
    private def check(name: String, term: Term, expected: Const)(using munit.Location): Unit =
        test(name):
            for (engine, outcome) <- evalAll(term) do
                assertEquals(outcome, Right(Some(expected)), s"engine: ${engine.label}")

    /** Asserts that all engines fail with an [[EvaluationError]]. */
    private def checkFails(name: String, term: Term)(using munit.Location): Unit =
        test(name):
            for (engine, outcome) <- evalAll(term) do
                assert(outcome.isLeft, s"${engine.label} should fail, got $outcome")

    private def integer(n: BigInt) = Const.Integer(n)

    // ---- programs ----
    check("fib 15", Examples.program("fib", 15), integer(610))
    check("factorial 30 (beyond Long)", Examples.program("factorial", 30),
      integer(BigInt("265252859812191058636308480000000")))
    check("sumTo 100", Examples.program("sumTo", 100), integer(5050))

    // ---- builtins ----
    check("add", add(int(2), int(3)), integer(5))
    check("subtract", sub(int(2), int(3)), integer(-1))
    check("multiply", mul(int(-4), int(3)), integer(-12))
    check("equals", eqInt(int(7), int(7)), Const.Bool(true))
    check("lessThanEquals", leq(int(8), int(7)), Const.Bool(false))
    check("big constant", add(int(BigInt("123456789012345678901234567890")), int(1)),
      integer(BigInt("123456789012345678901234567891")))

    // ---- builtins as first-class values ----
    check("partially applied builtin", lam("f")(v("f")(int(1), int(2)))(builtin(AddInteger)),
      integer(3))
    check("builtin passed one argument at a time",
      lam("inc")(v("inc")(int(41)))(builtin(AddInteger)(int(1))), integer(42))

    // ---- scoping ----
    check("shadowing", lam("x")(lam("x")(v("x"))(int(2)))(int(1)), integer(2))
    check("closure captures", lam("x")(lam("y")(v("x"))(int(2)))(int(1)), integer(1))

    // ---- case on bool ----
    check("case True selects branch 1", Case(bool(true), List(int(0), int(1))), integer(1))
    check("case False selects branch 0", Case(bool(false), List(int(0), int(1))), integer(0))
    check("case is lazy: the other branch never runs", ifThen(bool(true), int(1), error),
      integer(1))
    check("case False with a single branch", Case(bool(false), List(int(7))), integer(7))
    checkFails("case True with a single branch", Case(bool(true), List(int(7))))
    checkFails("case with three branches", Case(bool(true), List(int(0), int(1), int(2))))
    checkFails("case on an integer", ifThen(int(1), int(2), int(3)))
    checkFails("case scrutinee fails first", Case(error, List(int(0), int(1))))

    // ---- failures ----
    checkFails("error", error)
    checkFails("apply a constant", int(1)(int(2)))
    checkFails("add a bool", add(int(1), bool(true)))
    checkFails("over-applied builtin", builtin(AddInteger)(int(1), int(2), int(3)))

    // ---- the stack-depth trade-off ----
    test("CEK is stack-safe on deep recursion"):
        assertEquals(Engine.CEK.prepare(Examples.program("sumTo", 100_000))(),
          Some(integer(BigInt(100_000L) * 100_001 / 2)))
