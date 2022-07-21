object Test {
  def main(args: Array[String]): Unit = {
    val m = new javax.script.ScriptEngineManager(getClass().getClassLoader())
    val e = m.getEngineByName("scala").nn
    println(e.eval("42"))
    println(e.eval("Some(42)").asInstanceOf[Option[Int]].get)
    println(e.eval(new java.io.StringReader("42")))
  }
}
