package uplc.bench

import org.openjdk.jmh.annotations.*
import uplc.*

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/** Steady-state evaluation time of each engine on the example programs.
  *
  * Run: `sbt "bench/Jmh/run -i 5 -wi 5 -f 1 .*EngineBenchmark.*"`
  *
  * Compilation happens in `@Setup`, outside the measurement: this measures the code each engine
  * produces, not the cost of producing it (see [[CompileBenchmark]] for that).
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(1)
class EngineBenchmark:

    /** `name:n` pairs; kept small enough that the native-stack engines do not overflow. */
    @Param(Array("fib:20", "factorial:200", "sumTo:1000"))
    var program: String = uninitialized

    private var term: Term = uninitialized
    private var closures: () => Any = uninitialized
    private var jit: () => Any = uninitialized

    @Setup
    def setup(): Unit =
        val Array(name, n) = program.split(':')
        term = Examples.program(name, n.toInt)
        closures = ClosureCompiler.compile(term)
        jit = StagedJit.compile(term)

    @Benchmark def cek(): Value = Cek.evaluate(term)

    @Benchmark def closureCompiler(): Any = closures()

    @Benchmark def stagedJit(): Any = jit()

/** One-off cost of the staged JIT: generating, type-checking and compiling Scala to bytecode. This
  * is the price you pay up front, so the JIT pays off for scripts that are evaluated many times.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 5)
@Measurement(iterations = 5, time = 5)
@Fork(1)
class CompileBenchmark:

    @Benchmark def compileFib(): () => Any = StagedJit.compile(Examples.fib)
