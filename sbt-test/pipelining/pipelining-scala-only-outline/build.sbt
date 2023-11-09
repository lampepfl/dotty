ThisBuild / usePipelining := true

lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Yexperimental-outline",
    scalacOptions += "-Ymax-parallelism:1",
    scalacOptions += "-Ycheck:all",
  )

lazy val b = project.in(file("b"))
  .dependsOn(a)
  .settings(
    scalacOptions += "-Yexperimental-outline",
    scalacOptions += "-Ymax-parallelism:1",
    scalacOptions += "-Ycheck:all",
  )
