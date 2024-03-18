import scala.compiletime.ops.int

type Count0[N <: Int,T] <: Tuple = (N,T) match
  case (0,_)      => EmptyTuple
  case (N,String) => String *: Count0[int.-[N, 1], String]
  case (N,Int)    => Int *: Count0[int.-[N, 1], Int]
  case (N,Float)  => Float *: Count0[int.-[N, 1], Float]
  case (N,Double) => Double *: Count0[int.-[N, 1], Double]


type Count1[N <: Int,T] <: Tuple = (N,T) match
  case (0,T)      => EmptyTuple
  case (N,String) => String *: Count1[int.-[N, 1], String]
  case (N,Int)    => Int *: Count1[int.-[N, 1], Int]
  case (N,Float)  => Float *: Count1[int.-[N, 1], Float]
  case (N,Double) => Double *: Count1[int.-[N, 1], Double]

def t01 = summon[Count0[1, Int] =:= Int *: EmptyTuple ]
def t02 = summon[Count0[2, Int] =:= Int *: Int *: EmptyTuple]
def t03 = summon[Count0[3, Int] =:= Int *: Int *: Int *: EmptyTuple]
def t04 = summon[Count0[4, Int] =:= Int *: Int *: Int *: Int *: EmptyTuple]
def t05 = summon[Count0[5, Int] =:= Int *: Int *: Int *: Int *: Int *: EmptyTuple]

def t11 = summon[Count1[1, Int] =:= Int *: EmptyTuple ]
def t12 = summon[Count1[2, Int] =:= Int *: Int *: EmptyTuple]
def t13 = summon[Count1[3, Int] =:= Int *: Int *: Int *: EmptyTuple] // was: Fail from here
def t14 = summon[Count1[4, Int] =:= Int *: Int *: Int *: Int *: EmptyTuple]
def t15 = summon[Count1[5, Int] =:= Int *: Int *: Int *: Int *: Int *: EmptyTuple]
