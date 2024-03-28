ThisBuild / usePipelining := true

lazy val a = project.in(file("a"))
  .settings(
    scalacOptions += "-Ycheck:all",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  )
