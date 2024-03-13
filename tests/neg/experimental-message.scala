//> using options -Yno-experimental

import scala.annotation.experimental

@experimental("not yet stable")
def f() = ???

def g() = f() // error
