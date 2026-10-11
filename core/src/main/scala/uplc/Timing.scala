package uplc

/** Crude but honest timing for a live demo: warm up so the JVM JIT kicks in, then report the mean
  * time per call. Use the JMH benchmarks for numbers you publish.
  */
object Timing:

    /** Mean microseconds per call of `run`, after `warmupMillis` of warm-up. */
    def measure(run: () => Any, warmupMillis: Long = 2000, measureMillis: Long = 3000): Double =
        loop(run, warmupMillis * 1_000_000L)
        val (iterations, elapsed) = loop(run, measureMillis * 1_000_000L)
        elapsed / 1e3 / iterations

    private def loop(run: () => Any, durationNanos: Long): (Long, Long) =
        val start = System.nanoTime()
        var iterations = 0L
        var sink = 0
        while System.nanoTime() - start < durationNanos do
            sink += run().hashCode // consume the result so it is not optimized away
            iterations += 1
        if sink == 42 then print("") // keeps `sink` alive
        (iterations, System.nanoTime() - start)
