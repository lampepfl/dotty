ThisBuild / usePipelining := true

lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Ycheck:all",
  )

lazy val b = project.in(file("b"))
  .settings(
    scalacOptions += "-Ycheck:all",
  )

lazy val c = project.in(file("c"))
  .dependsOn(a, b)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
