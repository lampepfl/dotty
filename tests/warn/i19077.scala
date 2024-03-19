//> using options -deprecation

def f(@deprecatedName x: Int) = x * 2
def g(@deprecatedName("x") x: Int) = x * 2
def h(@deprecatedName("x") y: Int) = y * 2

@main def Test =
  f(x = 2) // warn
  g(x = 2) // warn
  h(x = 2) // warn
  h(y = 2) // no-warn