/*
extension (x: String = "123") // error
  def f2 = x.reverse
*/

extension (x: => String) // error
  def f2 = x.reverse
