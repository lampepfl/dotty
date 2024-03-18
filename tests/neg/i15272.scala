    sealed trait EdgeN[+NT]
    object EdgeN:
      case class Head[+NT, +From <: NT, +To <: NT]    (from: From, to: To ) extends EdgeN[NT]
      case class Cons[+NT, +From <: NT, +ToE <: EdgeN[NT]](from: From, to: ToE) extends EdgeN[NT]
      final type InNodesTupleOf[NT, E <: EdgeN[NT]] <: Tuple = E match
        case Cons[nt,from,toE] => from *: InNodesTupleOf[nt,toE] // error
        case Head[nt,from ,to] => from *: EmptyTuple
      def inNodesTuple[NT,E <: EdgeN[NT]](edge: E): InNodesTupleOf[NT,E] = edge match
        case e: Cons[nt,from,toE] => e.from *: inNodesTuple[nt,toE](e.to) // error
        case e: Head[nt,from,to] => e.from *: EmptyTuple
    end EdgeN
