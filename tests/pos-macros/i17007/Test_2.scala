//> using option "-Ycheck:all"
transparent inline def inlineMethod(v: Int) =
  locally(v)
  0

@annotation
class Test1:
  def method = inlineMethod(42)

object Test2:
  @annotation val valdef = inlineMethod(42)
  @annotation def defdef = inlineMethod(42)
