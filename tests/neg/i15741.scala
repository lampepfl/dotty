  def get(using Int): String = summon[Int].toString

  def pf2: PartialFunction[String, Int ?=> String] = {
    case "hoge" => get
    case "huga" => get
  } // error

  type IS = Int ?=> String

  def pf3: PartialFunction[String, IS] = {
    case "hoge" => get
    case "huga" => get
  } // error


