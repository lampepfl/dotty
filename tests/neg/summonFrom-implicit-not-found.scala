@annotation.implicitNotFound("there is no Missing!")
trait Missing

inline def summonMissing = compiletime.summonFrom {
  case m: Missing => m
}
inline def summonMissing2 = compiletime.summonInline[Missing]
val x = summonMissing // error
val y = summonMissing2 // error
