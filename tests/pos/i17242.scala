
class local(num: Int) extends annotation.StaticAnnotation
class local2(predicate: Boolean) extends annotation.StaticAnnotation

def working1(x: Int)(z: Int @local(x + x)) = ???
def working2(x: Int, z: Int @local(x)) = ???
def working3(x: Int, z: Int & x.type ) = ???


def failing1(x: Int, z: Int @local(x + x)) = ???    //error: undefined: x.+ # -1: TermRef(TermParamRef(x),+) at typer
def failing2(x: Int, z: Int @local2(x == x)) = ???  //error: undefined: x.== # -1: TermRef(TermParamRef(x),==) at typer
