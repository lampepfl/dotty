ThisBuild / usePipelining := true

// defines a macro, normally this would cause sbt not to write the early output jar, but we force it
// this will cause b to fail to compile due to the missing macro class,
// see `sbt-test/pipelining/pipelining-scala-macro` for how by default sbt does the right thing
lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Ycheck:all",
    Compile / incOptions := {
      val old = (Compile / incOptions).value
      val hooks = old.externalHooks
      val newHooks = hooks.withExternalLookup(
        new sbt.internal.inc.NoopExternalLookup {
          // force early output, this is safe in projects where the macro implementation is not in the same project,
          // however in this build, b will now fail as it will not find the macro implementation class.
          override def shouldDoEarlyOutput(analysis: xsbti.compile.CompileAnalysis): Boolean = true
        }
      )
      old.withExternalHooks(newHooks)
    },
  )

// uses the macro, this will fail because we forced early output ping, causing the missing macro implementation class
lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
