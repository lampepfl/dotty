
// test argument processing and "execution mode" in runner
object Test:
  import dotty.tools.runner.RichClassLoader.*
  val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(getClass.getClassLoader)
  def main(args: Array[String]): Unit =
    getClass.getClassLoader.nn.run("echo", List("hello", "raw", "world"))
    // caution: uses "SCALA_OPTS"
    dotty.tools.MainGenericRunner.main(Array("--class-path", classpath, "echo", "hello", "run", "world"))

@main def echo(args: String*): Unit = println {
  args.mkString(" ")
}
