object Test:
  type MyString = String

  def main(args: Array[String]): Unit =
    println(typeNameOf[MyString])
    println(typeNameOfF1 { (x: MyString) =>  })
    println(typeNameOfF1[MyString] { (x: MyString) =>  })
    println(typeNameOfF1 { (x: Seq[MyString]) => })
    println(typeNameOfF1 { (x: (MyString, Int)) => })
