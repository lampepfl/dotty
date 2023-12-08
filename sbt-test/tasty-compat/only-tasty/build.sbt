lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Youtput-only-tasty",
  )

lazy val b = project.in(file("b"))
  .settings(
    scalacOptions += "-Youtput-only-tasty",
    Compile / exportJars := true,
  )

lazy val c = project.in(file("c"))
  .dependsOn(a, b)
  .settings(
    scalacOptions += "-Ycheck:all",
  )
