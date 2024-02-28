import scala.quoted._
import scala.deriving._

def blah[P <: Product]
    (m: Mirror.ProductOf[P])
    (using Quotes, Type[m.MirroredElemLabels], Type[m.MirroredElemTypes]) = {
  type z = Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]
  Type.of[z] // error
  ()
}

def blah2[P <: Product, MEL <: Tuple: Type, MET <: Tuple: Type]
    (m: Mirror.ProductOf[P] { type MirroredElemLabels = MEL; type MirroredElemTypes = MET})
    (using Quotes) = {
  Type.of[Tuple.Zip[MEL, MET]]
  ()
}
