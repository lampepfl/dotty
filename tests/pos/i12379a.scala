transparent inline def convFail[Of, From](inline from : From) : Unit =
  val c = compiletime.summonInline[Conversion[from.type, Of]]

inline def convOK[Of, From](inline from : From)(using c : Conversion[from.type, Of]) : Unit = {}

class Bar[T](value : T)
given [T <: Int] : Conversion[T, Bar[T]] = Bar(_)

@main def main : Unit = {
  convOK[Bar[1],1](1)
  convFail[Bar[1],1](1) //causes error
}
