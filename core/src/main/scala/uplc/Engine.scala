package uplc

/** The three evaluation strategies behind one interface, so tests, demo and benchmarks treat them
  * uniformly. `prepare` does the one-off work (compilation); the returned function evaluates.
  */
enum Engine(val label: String):
    case CEK extends Engine("CEK machine")
    case Closures extends Engine("Closure compiler")
    case JIT extends Engine("Staged compiler")

    /** Prepares `term` for repeated evaluation. Each call of the result evaluates `term` once and
      * returns its value as a constant, or `None` if the result is not a constant (e.g. a lambda).
      */
    def prepare(term: Term): () => Option[Const] = this match
        case CEK => () => Cek.toConst(Cek.evaluate(term))
        case Closures =>
            val code = ClosureCompiler.compile(term)
            () => Runtime.toConst(code())
        case JIT =>
            val code = StagedJit.compile(term)
            () => Runtime.toConst(code())
