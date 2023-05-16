// Rewriting to indent should preserve comments
class A /* 1 */ { /* 2 */
  def m1(b: Boolean) = /* 3 */ { /* 4 */
    val x = if (b)
    /* 5 */ {
      "true"
    } /* 6 */
    else
    { /* 7 */
      "false"
    /* 8 */ }
/* 9 */ x.toBoolean
  /* 10 */ } /* 11 */
/* 12 */def m2 = {// 12
m1// 14
  /* 15 */{// 16
true
/* 17 */}// 18
// because of the missing indent before {
// the scanner inserts a new line between || and {
// cannot rewrite to indentation without messing the comments up
true ||// 19
/* 20 */{
  false
}// 21
}
}
