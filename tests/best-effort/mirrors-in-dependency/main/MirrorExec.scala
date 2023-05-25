import scala.deriving.Mirror

object MirrorExec:
  transparent inline def getNames[T](using m: Mirror.Of[T]): m.MirroredElemTypes =
    scala.compiletime.erasedValue[m.MirroredElemTypes]

  val ab = getNames[MirrorTypes.BrokenType]
