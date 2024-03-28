lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Yjava-tasty", // enable pickling of java signatures
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-result-types-java-tasty.jar").toString),
    scalacOptions += "-Ycheck:all",
    Compile / classDirectory := ((ThisBuild / baseDirectory).value / "a-result-types-classes"), // send classfiles to a different directory
  )

lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-result-types-java-tasty.jar")),
    scalacOptions += "-Ycheck:all",
  )
  .settings(
    fork := true, // we have to fork the JVM if we actually want to run the code with correct failure semantics
    Runtime / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-result-types-classes"), // make sure the java classes are visible at runtime
  )

// same as b, but adds the real classes to the classpath instead of the tasty jar
lazy val bAlt = project.in(file("b-alt"))
  .settings(
    Compile / sources := (b / Compile / sources).value,
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-result-types-classes")),
    scalacOptions += "-Ycheck:all",
  )
  .settings(
    fork := true, // we have to fork the JVM if we actually want to run the code with correct failure semantics
    Runtime / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-result-types-classes"), // make sure the java classes are visible at runtime
  )
