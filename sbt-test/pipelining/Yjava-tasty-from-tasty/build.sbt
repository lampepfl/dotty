// `a` contains mixed java/scala sources so sbt will send java sources to Scala compiler.
lazy val a = project.in(file("a"))
  .settings(
    compileOrder := CompileOrder.Mixed, // ensure we send java sources to Scala compiler
    scalacOptions += "-Yjava-tasty", // enable pickling of java signatures
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a-pre-java-tasty.jar").toString),
    scalacOptions += "-Ycheck:all",
    Compile / classDirectory := ((ThisBuild / baseDirectory).value / "a-pre-classes"), // send classfiles to a different directory
  )

// recompile `a` with `-from-tasty` flag to test idempotent read/write java signatures.
// Requires -Yjava-tasty to be set in order to read them.
lazy val a_from_tasty = project.in(file("a_from_tasty"))
  .settings(
    Compile / sources := Seq((ThisBuild / baseDirectory).value / "a-pre-java-tasty.jar"),
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-pre-java-tasty.jar")),
    scalacOptions += "-from-tasty", // read the jar file tasties as the source files
    scalacOptions += "-Yjava-tasty",
    scalacOptions += "-Yallow-outline-from-tasty", // allow outline signatures to be read with -from-tasty
    scalacOptions ++= Seq("-Yearly-tasty-output", ((ThisBuild / baseDirectory).value / "a_from_tasty-java-tasty.jar").toString),
    scalacOptions += "-Ycheck:all",
    Compile / classDirectory := ((ThisBuild / baseDirectory).value / "a_from_tasty-classes"), // send classfiles to a different directory
  )

lazy val b = project.in(file("b"))
  .settings(
    scalacOptions += "-Ycheck:all",
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a_from_tasty-java-tasty.jar")),
  )
  .settings(
    // we have to fork the JVM if we actually want to run the code with correct failure semantics
    fork := true,
    // make sure the java classes are visible at runtime
    Runtime / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-pre-classes"),
  )

// same as b, but adds the real classes to the classpath instead of the tasty jar
lazy val bAlt = project.in(file("b-alt"))
  .settings(
    scalacOptions += "-Ycheck:all",
    Compile / sources := (b / Compile / sources).value,
    Compile / unmanagedClasspath := Seq(Attributed.blank((ThisBuild / baseDirectory).value / "a-pre-classes")),
  )
  .settings(
    // we have to fork the JVM if we actually want to run the code with correct failure semantics
    fork := true,
    // make sure the java classes are visible at runtime
    Runtime / unmanagedClasspath += Attributed.blank((ThisBuild / baseDirectory).value / "a-pre-classes"),
  )
