object ABug:
  sealed trait Bit
  sealed trait B0 extends Bit
  sealed trait B1 extends Bit

  sealed trait Bin
  sealed trait Nil extends Bin
  sealed trait ::[U <: Bit, D <: Bin] extends Bin

  type Zero = B0 :: Nil
  type One  = B1 :: Nil

  type --[B <: Bin] <: Bin =
    B match
      case B1 :: d => B0 :: d
      case B0 :: B1 :: Nil => B1 :: Nil
      case B0 :: d => B1 :: --[d]

  type ×[N <: Bin, M <: Bin] <: Bin =
    (N, M) match
      case (Zero, ?) => Zero

  type ![N <: Bin] <: Bin =
    N match
      case Zero => One
      case One => One
      case _ => ![--[N]] × (N)
      case ? :: ? => ![--[N]] × (N)
