ThisBuild / usePipelining := true

// defines just the macro implementations
lazy val macros = project.in(file("macros"))
  .settings(
    scalacOptions += "-Ycheck:all",
    Compile / exportPipelining := false // downstream waits until classfiles are available
  )

// defines a macro, we need to force sbt to produce the early output jar
// because it will detect macros in the analysis.
// However the classes for the implementation are provided by `macros`
lazy val a = project.in(file("a"))
  .dependsOn(macros)
  .settings(
    scalacOptions += "-Ycheck:all",
    scalacOptions += "-Xprint-suspension",
    Compile / incOptions := {
      val old = (Compile / incOptions).value
      val hooks = old.externalHooks
      val newHooks = hooks.withExternalLookup(
        new sbt.internal.inc.NoopExternalLookup {
          // force early output, this is safe because the macro class from `macros` will be available.
          override def shouldDoEarlyOutput(analysis: xsbti.compile.CompileAnalysis): Boolean = {
            val internalClasses = analysis.asInstanceOf[sbt.internal.inc.Analysis].apis.internal
            val a_A = internalClasses.get("a.A")
            val a_AConsume = internalClasses.get("a.AConsume")
            val a_AConsumeTransparent = internalClasses.get("a.AConsumeTransparent")
            assert(a_A.exists(cls => cls.hasMacro), s"`a.A` wasn't found, or it didn't have a macro.")
            assert(a_AConsume.isDefined, s"`a.AConsume` wasn't found.")
            assert(a_AConsumeTransparent.isDefined, s"`a.AConsumeTransparent` wasn't found.")
            true // because `a.A` has macros, normally this would be false
          }
        }
      )
      old.withExternalHooks(newHooks)
    },
  )

// uses the macro, will still succeed as the macro implementation class is available
lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
