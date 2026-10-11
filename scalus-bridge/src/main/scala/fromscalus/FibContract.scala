package fromscalus

import scalus.compiler.Compile

/** Plain Scala, compiled to UPLC by the Scalus compiler plugin. `@Compile` makes the plugin keep
  * the method's intermediate representation (SIR) so `compile { ... }` can lower it to UPLC.
  */
@Compile
object FibContract:
    def fib(n: BigInt): BigInt = if n <= 1 then n else fib(n - 1) + fib(n - 2)
