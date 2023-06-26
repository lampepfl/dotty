inline trait A:
  final val x = 1

class B extends A:
  override final val x = 2 // error