package fromscalus

import _root_.uplc.{Const, Term}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.compile
import scalus.toUplcOptimized
import scalus.uplc.{Constant as SConst, Term as STerm}
import scalus.uplc.eval.PlutusVM

/** `fib`, from Scala source to every engine:
  * {{{
  * Scala ──Scalus──▶ UPLC ──FromScalus──▶ mini-UPLC ──▶ CEK | closures | staged JIT
  * }}}
  *
  * Note: no `import scalus.*` here — it would bring `scalus.uplc` into scope and shadow our own
  * top-level `uplc` package.
  */
object ScalusFib:

    /** Scala → SIR (Scalus' intermediate representation), done by the compiler plugin. */
    private val sir = compile((n: BigInt) => FibContract.fib(n))

    /** SIR → optimized UPLC, exactly what would go on-chain (minus the error traces). */
    val scalusUplc: STerm = sir.toUplcOptimized(generateErrorTraces = false)

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
