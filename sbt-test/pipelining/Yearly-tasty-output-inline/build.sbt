// defines a inline method
lazy val a = project.in(file("a"))
  .settings(
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-early.jar").toString),
    scalacOptions += "-Ystop-after:firstTransform",
    scalacOptions += "-Ycheck:all",
  )

// uses the inline method, this is fine as there is no macro classloader involved
lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-early.jar"),
    scalacOptions += "-Ycheck:all",
  )
