trait T {
  type L[X] = List[X]
  type T1 <: L    // was error: takes type parameters
  type T2 = L     // was error: takes type parameters
  type T3 = List  // was error: takes type parameters
  type T4 <: List // was error: takes type parameters
}
