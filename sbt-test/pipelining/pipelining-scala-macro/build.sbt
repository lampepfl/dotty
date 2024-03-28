ThisBuild / usePipelining := true

// defines a macro, sbt will not force the early output
// because it will detect macros in the analysis, so b will compile fine,
// see `sbt-test/pipelining/pipelining-scala-macro-fail` for how we can
// force a failure by always forcing early output.
lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Ycheck:all",
    scalacOptions += "-Xprint-suspension",
    Compile / incOptions := {
      val old = (Compile / incOptions).value
      val hooks = old.externalHooks
      val newHooks = hooks.withExternalLookup(
        new sbt.internal.inc.NoopExternalLookup {
          @volatile var knownSuspension = false

          def didFindMacros(analysis: xsbti.compile.CompileAnalysis) = {
            val foundMacros = analysis.asInstanceOf[sbt.internal.inc.Analysis].apis.internal.values.exists(_.hasMacro)
            assert(foundMacros, "expected macros to be found in the analysis.")
            foundMacros
          }

          // force early output, this is safe because the macro class from `macros` will be available.
          override def shouldDoEarlyOutput(analysis: xsbti.compile.CompileAnalysis): Boolean = {
            val internalClasses = analysis.asInstanceOf[sbt.internal.inc.Analysis].apis.internal
            val a_A = internalClasses.get("a.A")
            val a_ASuspendTyper = internalClasses.get("a.ASuspendTyper")
            val a_ASuspendInlining = internalClasses.get("a.ASuspendInlining")
            assert(a_A.isDefined, s"`a.A` wasn't found.")

            if (!knownSuspension) {
              // this callback is called multiple times, so we only want to assert the first time,
              // in subsequent runs the suspended definition will be "resumed", so a.ASuspendTyper be found.
              knownSuspension = true
              assert(a_ASuspendTyper.isEmpty, s"`a.ASuspendTyper` should have been suspended initially.")
            }

            assert(a_ASuspendInlining.isDefined, s"`a.ASuspendInlining` wasn't found.")

            // do what sbt does typically,
            // it will not force early output because macros are found
            !didFindMacros(analysis)
          }
        }
      )
      old.withExternalHooks(newHooks)
    },
  )

// uses the macro, sbt is smart enough to not use pipelining flags when upstream compilation has macros
lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
