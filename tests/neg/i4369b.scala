trait X[R <: Z, Z >: X[R, R] <: X[R, R]] // error
class Z extends X[Z, Z]
