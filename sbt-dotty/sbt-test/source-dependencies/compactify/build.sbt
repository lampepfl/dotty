TaskKey[Unit]("output-empty") := {
  val outputDirectory = (classDirectory in Compile).value
  val classes = (outputDirectory ** "*.class").get
  if (classes.nonEmpty) sys.error("Classes existed:\n\t" + classes.mkString("\n\t")) else ()
}

// apparently Travis CI stopped allowing long file names
// it fails with the default setting of 255 characters so
// we have to set lower limit ourselves
scalacOptions ++= Seq("-Xmax-classfile-name", "240")
