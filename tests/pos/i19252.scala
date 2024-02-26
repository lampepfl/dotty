//> using options -Werror -Wunused:imports
package foo {
  trait D1
}

object Bug:
  import foo.D1
  class Cl(d1: D1):
    import foo.D1
end Bug
