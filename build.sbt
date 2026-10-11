// ScalaDays 2026 — "Blazing-Fast Lambda Calculus via Metaprogramming"
//
// core          — mini-UPLC: AST, CEK machine, closure compiler, staged JIT, demo
// scalus-bridge — fib compiled from Scala by Scalus, run on our engines
// bench         — JMH benchmarks comparing the three engines

// scala3-staging must match the compiler version exactly: the staged code is
// compiled at runtime by the very same compiler that built this project.
// Scala 3.8+ needs JDK 17 or newer.
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "io.lantr"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

// JDK 24+ prints "terminally deprecated method in sun.misc.Unsafe" (JEP 498) the first time
// Scala's `LazyVals` (still Unsafe-based in 3.9) initializes a lazy val. The access is intended
// and harmless, so allow it in forked JVMs. The flag only exists since JDK 23 and older JVMs
// refuse to start with an unknown flag, hence the version check. Forked JVMs use the same JDK
// as sbt itself.
// sbt 1.x compiles build files with Scala 2.12: braces and `if (...)`, no Scala 3 syntax here.
lazy val allowSunMiscUnsafe: Seq[String] = {
    // "1.8" on Java 8, "11", "25", ... afterwards: the leading number is enough either way.
    val major = sys.props("java.specification.version").takeWhile(_.isDigit).toInt
    if (major >= 23) Seq("--sun-misc-unsafe-memory-access=allow") else Nil
}

lazy val root = project
    .in(file("."))
    .aggregate(core, scalusBridge, bench)
    .settings(publish / skip := true)

lazy val core = project
    .in(file("core"))
    .settings(
      name := "tiny-uplc",
      libraryDependencies ++= Seq(
        "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
        "org.scalameta" %% "munit" % "1.1.1" % Test
      ),
      // Staging spins up a compiler inside the running JVM: fork so it gets a
      // clean classloader and its own stack size settings.
      run / fork := true,
      Test / fork := true,
      // The demo intentionally shows a StackOverflowError on deep recursion,
      // so keep the default thread stack size here.
      run / javaOptions ++= Seq("-Xmx2g") ++ allowSunMiscUnsafe,
      Test / javaOptions ++= allowSunMiscUnsafe
    )

// Real compiler output: Scala → Scalus → UPLC → our engines. Kept out of `core` so the mini-UPLC
// stays dependency-free.
val scalusVersion = "1.3.0"

lazy val scalusBridge = project
    .in(file("scalus-bridge"))
    .dependsOn(core)
    .settings(
      name := "tiny-uplc-scalus-bridge",
      publish / skip := true,
      libraryDependencies ++= Seq(
        "org.scalus" %% "scalus" % scalusVersion,
        "org.scalameta" %% "munit" % "1.1.1" % Test
      ),
      // The plugin is published per exact compiler version (scalus-plugin_3.9.0).
      addCompilerPlugin("org.scalus" % "scalus-plugin" % scalusVersion cross CrossVersion.full),
      run / fork := true,
      Test / fork := true,
      run / javaOptions ++= allowSunMiscUnsafe,
      Test / javaOptions ++= allowSunMiscUnsafe
    )

lazy val bench = project
    .in(file("bench"))
    .dependsOn(core)
    .enablePlugins(JmhPlugin)
    .settings(
      name := "tiny-uplc-bench",
      publish / skip := true
    )
