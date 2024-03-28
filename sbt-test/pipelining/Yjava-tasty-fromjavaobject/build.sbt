lazy val a = project.in(file("a"))
  .settings(
    compileOrder := CompileOrder.Mixed, // ensure we send java sources to Scala compiler
    scalacOptions += "-Yjava-tasty", // enable pickling of java signatures
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-enum-java-tasty.jar").toString),
    scalacOptions += "-Ycheck:all",
    Compile / classDirectory := ((ThisBuild / baseDirectory).value / "a-enum-classes"), // send classfiles to a different directory
  )

// compiles the same sources as a, but with -Ytest-pickler
lazy val aCheck = project.in(file("a-check"))
  .settings(
    scalacOptions += "-Ytest-pickler", // check that the pickler is correct
    Compile / sources := (a / Compile / sources).value, // use the same sources as a
    compileOrder := CompileOrder.Mixed, // ensure we send java sources to Scala compiler
    scalacOptions += "-Yjava-tasty", // enable pickling of java signatures
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-enum-java-tasty-2.jar").toString),
    Compile / classDirectory := ((ThisBuild / baseDirectory).value / "a-enum-classes-2"), // send classfiles to a different directory
  )


lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-java-tasty.jar")),
    scalacOptions += "-Ycheck:all",
  )
  .settings(
    fork := true, // we have to fork the JVM if we actually want to run the code with correct failure semantics
    Runtime / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-classes"), // make sure the java classes are visible at runtime
  )

// same as b, but adds the real classes to the classpath instead of the tasty jar
lazy val bAlt = project.in(file("b-alt"))
  .settings(
    Compile / sources := (b / Compile / sources).value,
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-classes")),
    scalacOptions += "-Ycheck:all",
  )
  .settings(
    fork := true, // we have to fork the JVM if we actually want to run the code with correct failure semantics
    Runtime / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-classes"), // make sure the java classes are visible at runtime
  )

// negative compilation tests
lazy val c = project.in(file("c"))
  .settings(
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-java-tasty.jar")),
    scalacOptions += "-Ycheck:all",
  )

// same as c, but adds the real classes to the classpath instead of the tasty jar
lazy val cAlt = project.in(file("c-alt"))
  .settings(
    Compile / sources := (c / Compile / sources).value,
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-enum-classes")),
    scalacOptions += "-Ycheck:all",
  )
