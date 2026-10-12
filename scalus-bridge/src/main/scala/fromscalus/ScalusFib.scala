package fromscalus

import _root_.uplc.{Const, Term}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.{compile, Options}
import scalus.toUplcOptimized
import scalus.uplc.{Constant as SConst, Term as STerm}
import scalus.uplc.eval.PlutusVM

/** `fib`, from Scala source to every engine:
  * {{{
  * Scala ──Scalus──▶ UPLC ──FromScalus──▶ mini-UPLC ──▶ CEK | closures | staged compiler
  * }}}
  *
  * Note: no `import scalus.*` here — it would bring `scalus.uplc` into scope and shadow our own
  * top-level `uplc` package.
  */
object ScalusFib:

    /** Scala → SIR (Scalus' intermediate representation), done by the compiler plugin. */
    private val sir = compile((n: BigInt) => FibContract.fib(n))

    /** SIR → optimized UPLC with the release options (no error traces, traces removed), except
      * common-subexpression elimination. CSE would bind the shared `[fib fib]` to an extra lambda:
      * cheaper on-chain, but harder to read in a demo, so it is off (`cseIterations = 0`).
      */
    val scalusUplc: STerm =
        sir.toUplcOptimized(using Options.release.copy(cseIterations = 0))()

    /** The same program in our mini-UPLC. */
    val fib: Term = FromScalus.convert(scalusUplc)

    /** `fib n` as a closed program. */
    def program(n: Int): Term = Term.Apply(fib, Term.Constant(Const.Integer(n)))

    /** Scalus' production CEK machine, with Plutus V3 cost accounting. `case` on builtin values
      * needs protocol version 11 (van Rossem).
      */
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    /** Evaluates `fib n` with Scalus' own CEK machine. */
    def evaluateWithScalus(n: Int): STerm =
        STerm.Apply(scalusUplc, STerm.Const(SConst.Integer(n))).evaluate
