inline trait A:
  def x = 1
  def x_= (x: Int) = ???
  var y = 1

class B extends A