object Test {
  import scala.compiletime.constValue

  class Context {
    inline def assumeIn[T](op: => Context |=> T) = {
      instance of Context = this
      op
    }
  }

  def ctx: Context = new Context
  def g with Context = ()
  ctx.assumeIn(g)

/* The last three statements shoudl generate the following code:

    def ctx: Test.Context = new Test.Context()
    def g(implicit x$1: Test.Context): Unit = ()
    {
      val Context_this: Test.Context = Test.ctx
      {
        implicit def ctx: Test.Context = Context_this
        Test.g(ctx)
      }
    }
*/
}
