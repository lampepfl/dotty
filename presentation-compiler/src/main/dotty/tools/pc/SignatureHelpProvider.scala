package dotty.tools.pc

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.Signatures
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.MtagsEnrichments.*
import org.eclipse.lsp4j as l

import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.ReportContext
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch

object SignatureHelpProvider:

  def signatureHelp(
      driver: InteractiveDriver,
      params: OffsetParams,
      search: SymbolSearch
  )(using ReportContext): l.SignatureHelp =
    val uri = params.uri().nn
    val text = params.text().nn
    val sourceFile = SourceFile.virtual(uri, text)
    driver.run(uri.nn, sourceFile)

    driver.compilationUnits.get(uri) match
      case Some(unit) =>

        val pos = driver.sourcePosition(params, isZeroExtent = false)
        val path = Interactive.pathTo(unit.tpdTree, pos.span)(using driver.currentCtx)

        val localizedContext = Interactive.contextOfPath(path)(using driver.currentCtx)
        val indexedContext = IndexedContext(driver.currentCtx)

        given Context = localizedContext.fresh
          .setCompilationUnit(unit)
          .setPrinterFn(_ => ShortenedTypePrinter(search, IncludeDefaultParam.Never)(using indexedContext))

        val (paramN, callableN, alternatives) = Signatures.signatureHelp(path, pos.span)

        val infos = alternatives.flatMap: signature =>
          signature.denot.map(signature -> _)

        val signatureInfos = infos.map { case (signature, denot) =>
          search.symbolDocumentation(denot.symbol) match
            case Some(doc) =>
              withDocumentation(
                doc,
                signature,
                denot.symbol.is(Flags.JavaDefined)
              ).getOrElse(signature)
            case _ => signature

        }

        new l.SignatureHelp(
          signatureInfos.map(signatureToSignatureInformation).asJava,
          callableN,
          paramN
        )
      case _ => new l.SignatureHelp()
  end signatureHelp

  private def withDocumentation(
      info: SymbolDocumentation,
      signature: Signatures.Signature,
      isJavaSymbol: Boolean
  ): Option[Signatures.Signature] =
    val methodParams = info.parameters().nn.asScala
    val typeParams = info.typeParameters().nn.asScala

    def updateParams(params: List[Signatures.Param], typeParamIndex: Int, methodParamIndex: Int): List[Signatures.Param] =
      params match
        case (head: Signatures.MethodParam) :: tail =>
          val rest = updateParams(tail, typeParamIndex, methodParamIndex + 1)
          methodParams.lift(methodParamIndex) match
            case Some(paramDoc) =>
              val newName =
                if isJavaSymbol && head.name.startsWith("x$") then
                  paramDoc.nn.displayName()
                else head.name
              head.copy(name = newName.nn, doc = Some(paramDoc.docstring.nn)) :: rest
            case _ => head :: rest
        case (head: Signatures.TypeParam) :: tail =>
          val rest = updateParams(tail, typeParamIndex + 1, methodParamIndex)
          typeParams.lift(typeParamIndex) match
            case Some(paramDoc) =>
              head.copy(doc = Some(paramDoc.docstring.nn)) :: rest
            case _ => head :: rest
        case _ => Nil

    def updateParamss(
        params: List[List[Signatures.Param]],
        typeParamIndex: Int,
        methodParamIndex: Int
    ): List[List[Signatures.Param]] =
      params match
        case Nil => Nil
        case head :: tail =>
          val updated = updateParams(head, typeParamIndex, methodParamIndex)
          val (nextTypeParamIndex, nextMethodParamIndex) = head match
            case (_: Signatures.MethodParam) :: _ => (typeParamIndex, methodParamIndex + head.size)
            case (_: Signatures.TypeParam) :: _ => (typeParamIndex + head.size, methodParamIndex)
            case _ => (typeParamIndex, methodParamIndex)
          updated :: updateParamss(tail, nextTypeParamIndex, nextMethodParamIndex)
    val updatedParams = updateParamss(signature.paramss, 0, 0)
    Some(signature.copy(doc = Some(info.docstring().nn), paramss = updatedParams))
  end withDocumentation

  private def signatureToSignatureInformation(
      signature: Signatures.Signature
  ): l.SignatureInformation =
    val paramInfoss = (signature.paramss.flatten).map(paramToParameterInformation)
    val paramLists =
      signature.paramss
        .map { paramList =>
          val labels = paramList.map(_.show)
          val isImplicit = paramList.exists:
            case p: Signatures.MethodParam => p.isImplicit
            case _ => false
          val prefix = if isImplicit then "using " else ""
          val isTypeParams = paramList.forall(_.isInstanceOf[Signatures.TypeParam]) && paramList.nonEmpty
          val wrap: String => String = label => if isTypeParams then
            s"[$label]"
          else
            s"($label)"
          wrap(labels.mkString(prefix, ", ", ""))
        }.mkString


    val returnTypeLabel = signature.returnType.map(t => s": $t").getOrElse("")
    val label = s"${signature.name}$paramLists$returnTypeLabel"
    val documentation = signature.doc.map(markupContent)
    val sig = new l.SignatureInformation(label)
    sig.setParameters(paramInfoss.asJava)
    documentation.foreach(sig.setDocumentation(_))
    sig
  end signatureToSignatureInformation

  /**
   * Convert `param` to `ParameterInformation`
   */
  private def paramToParameterInformation(
      param: Signatures.Param
  ): l.ParameterInformation =
    val documentation = param.doc.map(markupContent)
    val info = new l.ParameterInformation(param.show)
    documentation.foreach(info.setDocumentation(_))
    info

  private def markupContent(content: String): l.MarkupContent | Null =
    if content.isEmpty() then null
    else
      val markup = new l.MarkupContent
      markup.setKind("markdown")
      markup.setValue(content.trim())
      markup

end SignatureHelpProvider
