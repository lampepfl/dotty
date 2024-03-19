trait x0 {

  type T = String { val x: Int = 1 }   // error: illegal refinement
  type U = String { def x(): Int = 1 } // error: illegal refinement
  type V = String { var x: Int = 1 }   // error: illegal refinement
}
