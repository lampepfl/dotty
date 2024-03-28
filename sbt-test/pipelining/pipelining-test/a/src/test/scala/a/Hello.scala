package a

import a.A

import org.junit.Test

class Hello {

  @Test def test(): Unit = {
    assert(A.foo == (1,2,3))
  }
}
