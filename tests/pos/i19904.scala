sealed trait Bx[AK1 <: AnyKind]
trait C1[AK2 <: AnyKind, X[_ <: AK2]]
trait C2[AK3 <: AnyKind, Y[_ <: AK3]]:
           def mm: C1[Bx[? <: AK3], [x <: Bx[? <: AK3]] =>> x match { case Bx[a] => Y[a] }]
class C3 extends C2[Any, [_] =>> Unit]:
  override def mm: C1[Bx[? <: Any], [y <: Bx[? <: Any]] =>> y match { case Bx[b] => Unit }] = ???

/*
C1[Bx[?], [y <: Bx[?]] =>> y match { case Bx[b] => Unit } <: Unit]  <:  C1[Bx[?], [x <: Bx[?]] =>> x match { case Bx[a] => Unit } <: Unit]
          [x <: Bx[?]] =>> x match { case Bx[a] => Unit } <: Unit   <:            [y <: Bx[?]] =>> y match { case Bx[b] => Unit } <: Unit
                           x match { case Bx[a] => Unit } <: Unit   <:                             x match { case Bx[b] => Unit } <: Unit
                             [a] =>> case Bx[a] => Unit             <:                    [b <: AnyKind] =>> case Bx[b] => Unit
                             [ <: AnyKind]                          <:                    []
                                  AnyKind                           <:                          Any  = false
*/
