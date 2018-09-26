import annotation.tailrec

object Test1772B {
  @tailrec
  def bar : Nothing = { // error: TailRec optimisation not applicable
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar // error: it is not in tail position
    } finally {
      bar // error: it is not in tail position
    }
  }

  @tailrec
  def baz : Nothing = { // error: TailRec optimisation not applicable
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => baz // error: it is not in tail position
    } finally {
      ???
    }
  }

  @tailrec
  def boz : Nothing = { // error: TailRec optimisation not applicable
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => boz; ??? // error: it is not in tail position
    }
  }

  @tailrec
  def bez : Nothing = { // error: TailRec optimisation not applicable
    try {
      bez // error: it is not in tail position
    } finally {
      ???
    }
  }

  // the `liftedTree` local method will prevent a tail call here.
  @tailrec
  def bar(i : Int) : Int = { // error: TailRec optimisation not applicable
    if (i == 0) 0
    else 1 + (try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar(i - 1) // old error: cannot rewrite recursive call
    })
  }
}
