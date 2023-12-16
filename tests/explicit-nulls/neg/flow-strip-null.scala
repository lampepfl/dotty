// Test we are correctly striping nulls from nullable unions.

class Foo:

  class B1
  class B2

  locally:
    val x: (Null | String) | Null |  (B1 | (Null | B2)) = ???
    if x != null then
      val _: String | B1 | B2 = x // ok: can remove all nullable unions

  locally:
    val x: (Null | String) & (Null | B1) = ???
    if x != null then
      val _: String & B1 = x // ok: can remove null from embedded intersection

  locally:
    val x: (Null | B1) & B2 = ???
    if x != null then
      val _: B1 & B2 = x // error: the type of x is not a nullable union, so we cannot remove the Null
