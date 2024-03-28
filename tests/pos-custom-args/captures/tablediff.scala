import language.experimental.captureChecking

trait Seq[+A]:
  def zipAll[A1 >: A, B](that: Seq[B]^, thisElem: A1, thatElem: B): Seq[(A1, B)]^{this, that}
  def map[B](f: A => B): Seq[B]^{this, f}

def zipAllOption[X](left: Seq[X], right: Seq[X]) =
  left.map(Option(_)).zipAll(right.map(Option(_)), None, None)

def fillRow[T](headRow: Seq[T], tailRow: Seq[T]) =
  val paddedZip = zipAllOption(headRow, tailRow)
