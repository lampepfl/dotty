//Test class
package test

type Bool = [R] => (R, R) => R
val True: Bool = [R] => (t: R, _: R) => t
val False: Bool = [R] => (_: R, f: R) => f

inline def not(b: Bool): Bool = ${notMacro('b)} // error
inline def show(b: Bool): String = ${showMacro('b)}
//inline def not(b: Bool): Bool = ${foldMacro('b, 'False, 'True)}
//inline def show(b: Bool): String = ${foldMacro('b, '{"TRUE"}, '{"FALSE"})}


@main def testing =
  println(show(not(True)))