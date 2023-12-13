//> using options -source future -language:experimental.modularity

trait Gen:
  private[Gen] val x: Any = ()

trait Year2(private[Year2] val value: Int) extends (Gen { val x: Int }) // error
