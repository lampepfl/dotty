import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*
import scala.collection.mutable.ArrayBuffer

@experimental
class data extends MacroAnnotation:
  import data.*
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    tree match
      case cdef: ClassDef =>
        val classPatches = ArrayBuffer[String]()

        val cls = tree.symbol
        val typeParams = cdef.body.map(_.symbol).filter(_.isType)
        val clsTpe =
          if typeParams.isEmpty then cls.typeRef
          else AppliedType(cls.typeRef, typeParams.map(_.typeRef))
        val expectedBody =
          clsTpe.asType match
            case '[t] => '{ data.generated[t]() }

        val params = paramNames(cls)
        for param <- params do
          val withParam = With(param)
          val paramType = cls.declaredField(param).info
          val existingOpt =
            cdef.body.find(stat =>
              val paramss = stat.symbol.paramSymss
              stat.symbol.name == withParam
              && paramss.size == 1 && paramss(0).size == 1
              && paramss(0)(0).name == param // FIXME: if the parameter name is incorrect, propose rewriting it
              && paramss(0)(0).info == paramType // FIXME: if the parameter type changed, propose rewriting it
            )
          existingOpt match
            case Some(tree: DefDef) =>
              tree.rhs match
                case Some(rhs) =>
                  if !rhs.asExpr.matches(expectedBody) then
                    report.error(s"Replace the underline code by:\n${expectedBody.show}", rhs.pos)
                case _ =>
                  report.error(s"Replace the underline code by:\n${tree.show} = ${expectedBody.show}", tree.pos)
            case _ =>
              // The method is not present
              classPatches +=
                s"def $withParam($param: ${paramType.show}): ${clsTpe.show} = ${expectedBody.show}"


        val ctr = cdef.constructor
        val endPos = Position(ctr.pos.sourceFile, ctr.pos.end, ctr.pos.end)
        // TODO: if the class has no existing body, we also need to add braces or ':'
        if classPatches.nonEmpty then
          report.error("@data requires the following additional method(s):\n\n" +
            classPatches.mkString("\n"), endPos)
      case _ =>
        report.error("Annotation only supports `class`")
    List(tree)

object data:
  // TODO: Is T necessary? Could this be transparent?
  inline def generated[T](): T = ${generatedImpl[T]}
  private def generatedImpl[T: Type](using Quotes): Expr[T] =
    import quotes.reflect.*
    val meth = Symbol.spliceOwner.owner
    val cls = meth.owner
    val params = paramNames(cls)
    meth.name match
      case With(paramName) =>
        val localParam = meth.paramSymss(0)(0)
        val body = // FIXME handle type parameters
          Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor),
            params.map(p => if p == paramName then Ref(localParam) else Ref(cls.declaredField(p))))
        body.asExprOf[T]
      case _ =>
        report.errorAndAbort("@data.generated used in invalid context")

  def paramNames(using Quotes)(cls: quotes.reflect.Symbol): List[String] =
    cls.primaryConstructor.paramSymss.dropWhile(_.headOption.exists(_.isType)).head.map(_.name)

private object With:
  def apply(paramName: String): String =
    s"with${paramName.head.toUpper}${paramName.tail}"
  def unapply(methodName: String): Option[String] = methodName match
      case s"with$rest" =>
        Some(s"${rest.head.toLower}${rest.tail}")
      case _ =>
        None
