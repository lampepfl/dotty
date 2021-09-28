package dotty.tools.scaladoc
package site


/**
 * Object for rendering yaml front-matter for preprocessed markdowns.
 */
object FrontMatterRenderer:
  def render(properties: Map[String, Object]): String =

    def renderProperties(newProps: Map[String, Object]): List[String] = newProps.collect {
      case (k: String, v: String) => s"$k: $v"
    }.toList

    val rows = renderProperties(properties) ++ renderProperties(properties("page").asInstanceOf[Map[String, Object]])

    rows.mkString("---\n", "\n", "\n---") + "\n\n<!-- THIS FILE HAS BEEN GENERATED BY SCALADOC PREPROCESSOR. " +
      "NOTE THAT ANY CHANGES TO THIS FILE CAN BE OVERRIDEN IN THE FUTURE -->\n\n"


