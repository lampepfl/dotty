package inlinetraits

val inlineValues: List[Int] =
  val c = C()
  List(c.zero, c.eleven, c.twelve, c.thirteen, c.twentyOne, c.twentyTwo, c.thirty)

inline trait T0:
  def zero: Int = 0
  def eleven: Int = 0
  def twelve: Int = 0
  def twentyOne: Int = 0
  def thirteen: Int = 0
  def twentyTwo: Int = 0
  def thirty: Int = 0

inline trait T11 extends T0:
  override def eleven: Int = 11
  override def twelve: Int = 11
  override def twentyOne: Int = 11
  override def thirteen: Int = 11
  override def twentyTwo: Int = 11
  override def thirty: Int = 11

inline trait T12 extends T0:
  override def twelve: Int = 12
  override def twentyOne: Int = 12
  override def thirteen: Int = 12
  override def twentyTwo: Int = 12
  override def thirty: Int = 12

inline trait T21 extends T11, T12:
  override def twentyOne: Int = 21
  override def thirteen: Int = 21
  override def twentyTwo: Int = 21
  override def thirty: Int = 21

inline trait T13 extends T0:
  override def thirteen: Int = 13
  override def twentyTwo: Int = 13
  override def thirty: Int = 13

inline trait T22 extends T12, T13:
  override def twentyTwo: Int = 22
  override def thirty: Int = 22

class C extends T21, T22:
  override def thirty: Int = 30
