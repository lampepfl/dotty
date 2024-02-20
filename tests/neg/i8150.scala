trait A
trait B
type T = {given A with A} // error
type T = {given(using a: A) as B} // error // error
// error