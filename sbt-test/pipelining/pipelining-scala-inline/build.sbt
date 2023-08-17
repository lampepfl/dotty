ThisBuild / usePipelining := true

// defines a purely inline function, and we always force the early output, this should not be needed in practice
// because pure inline methods do not have a Macro flag.
lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Ycheck:all",
    Compile / incOptions := {
      val old = (Compile / incOptions).value
      val hooks = old.externalHooks
      val newHooks = hooks.withExternalLookup(
        new sbt.internal.inc.NoopExternalLookup {
          // assert that the analysis contains the class `a.A` and that it does not have a macro.
          override def shouldDoEarlyOutput(analysis: xsbti.compile.CompileAnalysis): Boolean = {
            val internalClasses = analysis.asInstanceOf[sbt.internal.inc.Analysis].apis.internal
            val a_A = internalClasses.get("a.A")
            assert(a_A.exists(cls => !cls.hasMacro), "`a.A` wasn't found, or it had a macro.")

            // returning true will force the early output ping and activate downstream pipelining,
            // this is fine for inline methods, but see `sbt-test/pipelining/pipelining-scala-macro-fail` for how
            // we can force a failure by returning true here.
            true
          }
        }
      )
      old.withExternalHooks(newHooks)
    },
  )

// uses the purely inline function
lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
