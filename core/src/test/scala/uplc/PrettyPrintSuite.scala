package uplc

import munit.FunSuite

class PrettyPrintSuite extends FunSuite:
    import Dsl.*

    test("UPLC concrete syntax"):
        val term = lam("x")(add(v("x"), int(1)))
        assertEquals(term.show, "(lam x [(builtin addInteger) x (con integer 1)])")

    test("case is printed with the else branch first"):
        assertEquals(
          ifThen(bool(true), int(1), error).show,
          "(case (con bool True) (error) (con integer 1))"
        )

    test("recursion by self-application, like Scalus"):
        assertEquals(recursive("f")(v("f")).show, "[(lam f [f f]) (lam f f)]")

    test("generated Scala code turns case into a native if"):
        val code = StagedJit.showCode(Examples.program("fib", 10))
        assert(code.contains("if "), code)
        assert(!code.contains("caseIndex"), code)
