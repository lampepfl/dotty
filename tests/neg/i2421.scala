inline object Foo // OK (error would be detected later, in PostTyper)
inline class Bar // error: modifier(s) `inline' incompatible with type definition
inline abstract class Baz // error: modifier(s) `inline' incompatible with type definition

object Quux {
  inline type T // error: modifier(s) `inline' incompatible with type definition
  inline var x: 42 = 42 // error: modifier(s) `inline' incompatible with var definition
  inline lazy val y: 43 = 43 // error: modifier(s) `inline' incompatible with lazy val definition
}
