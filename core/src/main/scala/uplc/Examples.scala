package uplc

/** Example programs, written the way Scalus compiles Scala: recursion by self-application and
  * conditionals as `case` on a Bool. `fib` here is the same term Scalus produces for
  * {{{
  * def fib(n: BigInt): BigInt = if n <= 1 then n else fib(n - 1) + fib(n - 2)
  * }}}
  * (see the `scalus-bridge` module, which compiles that Scala code and runs the real output).
  */
object Examples:
    import Dsl.*

    /** fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) */
    val fib: Term = recursive("fib")(
      lam("n")(
        ifThen(
          leq(v("n"), int(1)),
          v("n"),
          add(self("fib", sub(v("n"), int(1))), self("fib", sub(v("n"), int(2))))
        )
      )
    )

    /** factorial n = if n <= 0 then 1 else n * factorial (n - 1) — overflows a Long at 21! */
    val factorial: Term = recursive("fact")(
      lam("n")(
        ifThen(leq(v("n"), int(0)), int(1), mul(v("n"), self("fact", sub(v("n"), int(1)))))
      )
    )

    /** sumTo n = if n <= 0 then 0 else n + sumTo (n - 1) — NOT tail recursive: its depth grows
      * with n, which is how we show the stack-depth pitfall of compiling to native JVM calls.
      */
    val sumTo: Term = recursive("sum")(
      lam("n")(
        ifThen(leq(v("n"), int(0)), int(0), add(v("n"), self("sum", sub(v("n"), int(1)))))
      )
    )

    /** Programs used by the demo and the benchmarks, by name. */
    def program(name: String, n: Int): Term = name match
        case "fib"       => fib(int(n))
        case "factorial" => factorial(int(n))
        case "sumTo"     => sumTo(int(n))
        case other       => throw IllegalArgumentException(s"unknown example: $other")
