inline trait A:
	extension [T](x: T)
		def foo[U](y: U)(z: T): (T, U, T) = (x, y, z)

class B extends A:
	def f = 1.foo("2")(3)