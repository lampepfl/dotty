lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Yjava-tasty", // enable pickling of java signatures
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-annotation-java-tasty.jar").toString),
    scalacOptions += "-Ycheck:all",
    Compile / classDirectory := ((ThisBuild / baseDirectory).value / "a-annotation-classes"), // send classfiles to a different directory
  )

lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-annotation-java-tasty.jar")),
    scalacOptions += "-Ycheck:all",
  )

// same as b, but adds the real classes to the classpath instead of the tasty jar
lazy val bAlt = project.in(file("b-alt"))
  .settings(
    Compile / sources := (b / Compile / sources).value,
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-annotation-classes")),
    scalacOptions += "-Ycheck:all",
  )
