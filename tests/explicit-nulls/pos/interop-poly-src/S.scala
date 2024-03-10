// Test the handling of generics by the nullability transform.
// There are two classes here: JavaCat is Java-defined, and ScalaCat
// is Scala-defined.

class ScalaCat[T] {}

class Test {
  // It's safe to return a JavaCat[String]|Null (no inner |Null),
  // because JavaCat, being a Java class, _already_ nullifies its
  // fields.
  val jc: JavaCat[String]|Null = J.getJavaCat[String]()
  val jc2: JavaCat[String] = J.getJavaCat[String]()
  // ScalaCat is Scala-defined, so we need the inner |Null.
  val sc: ScalaCat[String|Null]|Null = J.getScalaCat[String]()
  val sc2: ScalaCat[String]|Null = J.getScalaCat[String]()
  val sc3: ScalaCat[String|Null] = J.getScalaCat[String]()
  val sc4: ScalaCat[String] = J.getScalaCat[String]()

  import java.util.List

  val las: List[Array[String|Null]]|Null = J.getListOfStringArray()
  val las2: List[Array[String|Null]] = J.getListOfStringArray()
  val las3: List[Array[String]]|Null = J.getListOfStringArray()
  val las4: List[Array[String]] = J.getListOfStringArray()
  val als: Array[List[String]|Null]|Null = J.getArrayOfStringList()
  val als2: Array[List[String]|Null] = J.getArrayOfStringList()
  val als3: Array[List[String]]|Null = J.getArrayOfStringList()
  val als4: Array[List[String]] = J.getArrayOfStringList()
  val css: List[Array[List[Array[String|Null]]|Null]]|Null = J.getComplexStrings()
  val css2: List[Array[List[Array[String]]|Null]]|Null = J.getComplexStrings()
  val css3: List[Array[List[Array[String|Null]]]]|Null = J.getComplexStrings()
  val css4: List[Array[List[Array[String|Null]]|Null]] = J.getComplexStrings()
  val css5: List[Array[List[Array[String|Null]]]] = J.getComplexStrings()
  val css6: List[Array[List[Array[String]]]]|Null = J.getComplexStrings()
  val css7: List[Array[List[Array[String]]|Null]] = J.getComplexStrings()
  val css8: List[Array[List[Array[String]]]] = J.getComplexStrings()
}
