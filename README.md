# Blazing-Fast Lambda Calculus via Metaprogramming

Companion code for the ScalaDays 2026 talk by Alex Nemish (Lantr).

It contains the smallest subset of Untyped Plutus Core (UPLC) — the smart contract language of
Cardano — that runs real compiler output, and three ways to run it, each one tier faster than the last:

| Engine | File | Idea |
|---|---|---|
| CEK machine | `core/.../Cek.scala` | The textbook abstract machine: a loop over (Control, Environment, Kontinuation). Stack-safe. |
| Closure compiler | `core/.../ClosureCompiler.scala` | Translate the term once into a tree of Scala closures. |
| Staged compiler | `core/.../StagedJit.scala` | Generate Scala code with quotes and splices, compile it at runtime with `scala3-staging`, and let the JVM JIT do the rest. |

The `scalus-bridge` module compiles a plain Scala `fib` to UPLC with
[Scalus](https://github.com/scalus3/scalus), converts it to the mini-UPLC and runs it on all three
engines, checking the result against Scalus' own CEK machine. The production version of the JIT
— all of Plutus V3, exact cost accounting — lives in Scalus (`scalus-uplc-jit-compiler`).

## Mini-UPLC

```
t ::= x | (lam x t) | [t t] | (con integer n) | (con bool b)
    | (builtin f) | (case t t t) | (error)
f ::= addInteger | subtractInteger | multiplyInteger | equalsInteger | lessThanEqualsInteger
```

Exactly what Scalus emits for `fib`: recursion by self-application (`[fib fib]`) and conditionals
as `case` on a builtin Bool (lazy: `False` selects branch 0, `True` branch 1).

## Run

```bash
sbt test                                    # all engines agree with the CEK machine, incl. on failures
sbt "scalusBridge/run 20"                   # the live demo: Scala → Scalus → UPLC → our engines
sbt "core/run fib 25"                       # hand-written examples + the stack-depth pitfall
sbt "bench/Jmh/run .*EngineBenchmark.*"     # steady-state evaluation time
sbt "bench/Jmh/run .*CompileBenchmark.*"    # staged compilation cost
```

`core/run` ends with deep recursion (`sumTo 100000`): the CEK machine succeeds, the two compilers
overflow the JVM stack. Scalus solves this with a hybrid engine that falls back to a
heap-allocated continuation representation.
