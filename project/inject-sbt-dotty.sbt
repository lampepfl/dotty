// Include the sources of the sbt-dotty plugin in the project build,
// so that we can use the current in-development version of the plugin
// in our build instead of a released version.

unmanagedSourceDirectories in Compile += baseDirectory.value / "../sbt-dotty/src"

// Keep in sync with `sbt-dotty` config in Build.scala
libraryDependencies ++= Seq(
  Dependencies.`jackson-databind`,
  "org.scala-sbt" % "compiler-interface" % "1.0.0-X16"
)
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config"
