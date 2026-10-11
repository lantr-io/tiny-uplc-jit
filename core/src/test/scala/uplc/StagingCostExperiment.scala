package uplc

import scala.quoted.*

/** Measures how code generation time grows with term size, for two generators that differ only in
  * how they use `Quotes`.
  *
  *   - `inner` (the way of [[StagedJit]]): `gen` takes the `Quotes` of the splice it runs in, and
  *     each child is generated inside its splice. Every tree is created with its final owner.
  *   - `outer` (the way the Scalus JIT did it before the fix): `gen` captures the outermost
  *     `Quotes`, builds the children first, and splices the finished `Expr`s under `val`s and
  *     lambdas. Staging copies each such `Expr` to change its owner, and the reflect `Lambda` needs
  *     an explicit `changeOwner` of the whole body.
  *
  * Term shapes, all with about `n` nodes:
  *   - `lambdas`: `λx1. λx2. … λxn. [add x1 xn]`, lambda nesting depth `n`;
  *   - `lets`: `[(λx1. [(λx2. … ) (con 2)]) (con 1)]`, the let spine of compiled UPLC, depth `n`;
  *   - `wide`: a balanced tree of additions, depth `log2 n`.
  *
  * Run: `sbt "core/Test/runMain uplc.StagingCostExperiment"`.
  */
object StagingCostExperiment:
    import Term.*
    import DefaultFun.*

    // ---- term shapes ----

    def lambdas(n: Int): Term =
        val body: Term = Apply(Apply(Builtin(AddInteger), Var("x1")), Var(s"x$n"))
        (n to 1 by -1).foldLeft(body)((b, i) => LamAbs(s"x$i", b))

    def lets(n: Int): Term =
        val body: Term = Apply(Apply(Builtin(AddInteger), Var("x1")), Var(s"x$n"))
        (n to 1 by -1).foldLeft(body)((b, i) =>
            Apply(LamAbs(s"x$i", b), Constant(Const.Integer(i)))
        )

    def wide(n: Int): Term =
        if n <= 1 then Constant(Const.Integer(1))
        else
            val rest = (n - 3) max 0
            Apply(Apply(Builtin(AddInteger), wide(rest / 2 + 1)), wide(rest - rest / 2 + 1))

    def nodes(t: Term): Int = t match
        case LamAbs(_, b)  => 1 + nodes(b)
        case Apply(f, a)   => 1 + nodes(f) + nodes(a)
        case Case(s, bs)   => 1 + nodes(s) + bs.map(nodes).sum
        case _             => 1

    def depth(t: Term): Int = t match
        case LamAbs(_, b)  => 1 + depth(b)
        case Apply(f, a)   => 1 + math.max(depth(f), depth(a))
        case _             => 1

    // ---- generator 1: children generated inside their splices ----

    def inner(term: Term)(using Quotes): Expr[() => Any] =
        def gen(t: Term, env: Map[String, Expr[Any]])(using Quotes): Expr[Any] = t match
            case Var(n)       => env(n)
            case LamAbs(n, b) => '{ (arg: Any) => ${ gen(b, env.updated(n, 'arg)) } }
            case Apply(Apply(Builtin(AddInteger), a), b) =>
                '{
                    val x = ${ gen(a, env) }
                    val y = ${ gen(b, env) }
                    Runtime.asInteger(x) + Runtime.asInteger(y)
                }
            case Apply(f, a) =>
                '{
                    val g = ${ gen(f, env) }
                    Runtime.apply(g, ${ gen(a, env) })
                }
            case Constant(Const.Integer(i)) => '{ BigInt(${ Expr(i.toLong) }) }
            case other => throw IllegalArgumentException(other.show)
        '{ () => ${ gen(term, Map.empty) } }

    // ---- generator 2: children built first with the outermost Quotes ----

    def outer(term: Term)(using q: Quotes): Expr[() => Any] =
        import q.reflect.{Lambda, MethodType, Symbol, TypeRepr, asTerm}
        def gen(t: Term, env: Map[String, q.reflect.Term]): Expr[Any] = t match
            case Var(n) => env(n).asExprOf[Any]
            case LamAbs(n, b) =>
                val tpe = MethodType(List(n))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Any])
                Lambda(
                  Symbol.spliceOwner,
                  tpe,
                  { case (meth, args) =>
                      val arg = args.head.asInstanceOf[q.reflect.Term]
                      gen(b, env.updated(n, arg)).asTerm.changeOwner(meth)
                  }
                ).asExprOf[Any]
            case Apply(Apply(Builtin(AddInteger), a), b) =>
                val ea = gen(a, env)
                val eb = gen(b, env)
                '{
                    val x = $ea
                    val y = $eb
                    Runtime.asInteger(x) + Runtime.asInteger(y)
                }
            case Apply(f, a) =>
                val ef = gen(f, env)
                val ea = gen(a, env)
                '{
                    val g = $ef
                    Runtime.apply(g, $ea)
                }
            case Constant(Const.Integer(i)) => '{ BigInt(${ Expr(i.toLong) }) }
            case other => throw IllegalArgumentException(other.show)
        '{ () => ${ gen(term, Map.empty) } }

    // ---- measurement ----

    /** Generation time and total `staging.run` time in ms, with a fresh compiler. */
    def time(term: Term, generator: Quotes ?=> Expr[() => Any]): (Double, Double, Any) =
        given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
        var genNs = 0L
        val start = System.nanoTime()
        val fn = staging.run {
            val t0 = System.nanoTime()
            val e = generator
            genNs = System.nanoTime() - t0
            e
        }
        val total = System.nanoTime() - start
        (genNs / 1e6, total / 1e6, fn())

    private def onBigStack[A](body: => A): A =
        var result: Option[A] = None
        var error: Throwable = null
        val t = new Thread(null, () => try result = Some(body) catch case e: Throwable => error = e,
          "experiment", 2L << 30)
        t.start()
        t.join()
        if error != null then throw error
        result.get

    def main(args: Array[String]): Unit = onBigStack {
        val sizes = if args.nonEmpty then args.map(_.toInt).toList else List(100, 250, 500, 1000, 2000)
        // Warm up the compiler classes and the JVM JIT.
        for _ <- 1 to 3 do
            time(lets(200), inner(lets(200)))
            time(lets(200), outer(lets(200)))
        println("shape,generator,n,nodes,depth,gen_ms,total_ms")
        for
            (shapeName, shape) <- List(
              ("lambdas", lambdas),
              ("lets", lets),
              ("wide", wide)
            )
            n <- sizes
            (genName, generator) <- List(
              ("inner", (t: Term) => (q: Quotes) ?=> inner(t)),
              ("outer", (t: Term) => (q: Quotes) ?=> outer(t))
            )
        do
            val term = shape(n)
            val (genMs, totalMs, _) = time(term, generator(term))
            println(f"$shapeName,$genName,$n,${nodes(term)},${depth(term)},$genMs%.1f,$totalMs%.1f")
    }
