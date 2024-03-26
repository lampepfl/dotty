//> using options -Xfatal-warnings -Wdollar-check

/* vals */
val goodVal = 1
val $startVal = 1 // error
val mid$dleVal = 1 // error
val endVal$ = 1 // error

def testValUsage =
  $startVal + endVal$ // ok, should only warn on declaration not on usage

/* functions */
def $funcStart() = 3 // error
def func$Middle() = 3 // error
def funcEnd$() = 3 // error

def func1badArg(goodArg: Int, bad$Arg: Int) = 5 // error
def func1badArgAndUsageDoesNotThrowWarning(goodArg: Int, bad$Arg: Int) = bad$Arg // error
def func2badArgs(goodArg: Int, bad$Arg: Int, bad$arg2: String) = 5 // error // error
def multilineFunc(
  goodArg: Int,
  badAr$g: Int // error
) = 1

def testFuncUsaage =
  $funcStart() + func1badArg(1, 2) // ok, should only warn on declaration not on usage

/* types */
type GoodType = Int
type $StartType = Int // error
type Middle$Type = Int // error
type EndType$ = Int // error

val typedVal: Middle$Type = 2 // ok, should only warn on declaration not on usage
def funcWithDollarTypes(foo: $StartType): Middle$Type = 2 // ok, should only warn on declaration not on usage

/* enums */
enum GoodEnum:
  case GoodCase
  case $BadCaseStart // error
  case BadCase$Middle // error
  case BadCaseEnd$ // error

enum $BadEnumStart: // error
  case GoodCase
  case $BadCase // error

enum BadEnum$Middle: // error
  case GoodCase
  case Bad$Case // error

enum BadEnumEnd$: // error
  case GoodCase
  case BadCase$ // error

enum E$numWithEndKeyword: // error
  case SomeCase
end E$numWithEndKeyword // ok

def TestEnumUsage(a: $BadEnumStart): Int = // ok, should only warn on declaration not on usage
  a match
    case $BadEnumStart.GoodCase => 1
    case $BadEnumStart.$BadCase => 2 // ok, should only warn on declaration not on usage

/* objects */
object $ObjectStart: // error
  val goodVal = 1
  val $badVal = 2 // error

object Object$Middle: // error
  val goodVal = 1
  val bad$Val = 2 // error

object ObjectEnd$: // error
  val goodVal = 1
  val badVal$ = 2 // error

object GoodObject:
  val goodVal = 1
  val b$adVal = 2 // error

object Ob$jectWithEndKeyword: // error
  val someVal = 1
end Ob$jectWithEndKeyword // ok

val testObjectUsage = ObjectEnd$.badVal$ // ok, should only warn on declaration not on usage

/* case classes */
case class $InlineCaseClassStart(someField: Int) // error
case class InlineCaseClass$Middle(someField: Int) // error
case class InlineCaseClassEnd$(someField: Int) // error

case class InlineCaseClass(goodField: Int, badFiel$d: Int) // error

case class $CaseClassStart( // error
  somefield: Int,
  b$adfield: Int // error
)

case class CaseClass$Middle( // error
  somefield: Int,
  bad$Field: Int // error
)

case class CaseClassEnd$( // error
  somefield: Int,
  badField$: Int // error
)

// companion oject
object CaseClassEnd$: // error
  val food = 1

val testCaseClassUsage = CaseClass$Middle(somefield = 1, bad$Field = 2) // ok, should only warn on declaration not on usage

/* classes */
class GoodClass
class $StartClass // error
class Middle$Class // error
class EndClass$ // error

class Cla$$( // error
   var goodMember: Int,
   var badM$ember: Int // error
):
  def goodMethod(x: Int) = badM$ember // ok, only checking if the method name does not contain a dollar sign
  def bad$Method(y: Int) = goodMember // error
  def methodWithBadArgNames(b$ad$arg: Int) = goodMember // error
  def method$WithEndKeyword() = // error
    3
  end method$WithEndKeyword
end Cla$$ // ok

def testUsage =
  val instatiation = new Cla$$(goodMember = 1, badM$ember = 2) // ok, should only warn on declaration not on usage
  instatiation.bad$Method(1) // ok, should only warn on declaration not on usage
  instatiation.methodWithBadArgNames(2) // ok, should only warn on declaration not on usage


/* traits */
trait GoodTrait
trait $BadTraitStart // error
trait BadTrait$Middle // error
trait BadTraitEnd$ // error

class TestTraitUsage extends $BadTraitStart // ok, should only warn on declaration not on usage

package GoodPackage:
  val goodVal = 1
  val b$adVal = 2 // error

package $BadPackageStart: // error
  val goodVal = 1
  val $badVal = 2 // error

package BadPackage$Middle: // error
  val goodVal = 1
  val bad$Val = 2 // error

package BadPackageEnd$: // error
  val goodVal = 1
  val badVal$ = 2 // error