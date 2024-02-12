package dotty.tools.scaladoc
package renderers

import scala.collection.immutable.SortedMap
import scala.util.chaining.*
import util.HTML.{div, *}

import scala.jdk.CollectionConverters.*
import dotty.tools.scaladoc.translators.FilterAttributes
import org.jsoup.Jsoup
import translators.*
import java.net.URL
import scala.util.Try
import java.nio.file.{Files, Paths}

class MemberRenderer(signatureRenderer: SignatureRenderer)(using DocContext) extends DocRender(signatureRenderer):
  import signatureRenderer._

  val signatureProvider: ScalaSignatureProvider = ScalaSignatureProvider()

  def doc(m: Member): Seq[AppliedTag] =  m.docs.fold(Nil)(d => Seq(renderDocPart(d.body)))

  def tableRow(name: String, content: TagArg*) = Seq(dt(cls := "body-small")(name), dd(cls := "body-medium")(content*))

  def defintionClasses(m: Member) = m.origin match
    case Origin.Overrides(defs) =>
      def renderDef(d: Overridden): Seq[TagArg] =
        Seq(" -> ", signatureRenderer.renderLink(d.name, d.dri))
      val headNode = m.inheritedFrom.map(form => signatureRenderer.renderLink(form.name, form.dri))
      val tailNodes = defs.flatMap(renderDef)
      val nodes = headNode.fold(tailNodes.drop(1))(_ +: tailNodes)
      tableRow("Definition Classes", div(nodes*))

    case _ => Nil

  def inheritedFrom(m: Member) = m.inheritedFrom match
    case Some(InheritedFrom(name, dri, isSourceSuperclassHidden)) =>
      val hiddenNameSuffix = if isSourceSuperclassHidden then " (hidden)" else ""
      tableRow("Inherited from:", signatureRenderer.renderLink(name + hiddenNameSuffix, dri))
    case _ => Nil

  def flattenedDocPart(on: SortedMap[String, DocPart]): Seq[AppliedTag] =
    on.flatMap { case (name, value) => tableRow(name, renderDocPart(value)) }.toSeq

  def docAttributes(m: Member): Seq[AppliedTag] =

    def list(name: String, on: List[DocPart]): Seq[AppliedTag] =
      if on.isEmpty then Nil else tableRow(name, div(on.map(e => div(renderDocPart(e)))))

    def opt(name: String, on: Option[DocPart]): Seq[AppliedTag] =
      if on.isEmpty then Nil else tableRow(name, renderDocPart(on.get))

    def authors(authors: List[DocPart]) = if summon[DocContext].args.includeAuthors then list("Authors:", authors) else Nil

    m.docs.fold(Nil)(d =>
      opt("Returns", d.result) ++
      list("Throws", d.throws) ++
      opt("Constructor", d.constructor) ++
      authors(d.authors) ++
      list("See also", d.see) ++
      opt("Version", d.version) ++
      opt("Since", d.since) ++
      list("Todo", d.todo) ++
      list("Note", d.note) ++
      list("Example", d.example)
    )

  def companion(m: Member): Seq[AppliedTag] = m.companion.fold(Nil){ (kind, dri) =>
    val kindName = kind.name
    tableRow("Companion", signatureRenderer.renderLink(kindName, dri))
  }

  def source(m: Member): Seq[AppliedTag] =
    summon[DocContext].sourceLinks.pathTo(m).fold(Nil){ link =>
      tableRow("Source", a(href := link, target := "_blank")(m.sources.fold("(source)")(_.path.getFileName().toString())))
    }

  def deprecation(m: Member): Seq[AppliedTag] = m.deprecated.fold(Nil){ a =>
    def stripQuotes(s: String) = s.stripPrefix("\"").stripSuffix("\"")
    def parameter(p: Annotation.AnnotationParameter): TagArg = p match
      case Annotation.PrimitiveParameter(_, value) => stripQuotes(value)
      case Annotation.LinkParameter(_, dri, text) =>
        signatureRenderer.renderLink(stripQuotes(text), dri)
      case Annotation.UnresolvedParameter(_, value) => stripQuotes(value)

    val (named, unnamed) = a.params.partition(_.name.nonEmpty)
    val message = named.find(_.name.get == "message")
    val since = named.find(_.name.get == "since")

    val content = (
      Seq(
        since.map(s => code("[Since version ", parameter(s), "] ")),
        message.map(m => parameter(m)),
      ) ++ m.docs.map(_.deprecated.toSeq.map(renderDocPart))
      ).flatten.pipe { c =>
        if c.isEmpty then Seq("true") else c
      }
    tableRow("Deprecated", content*)
  }

  def experimental(m: Member) = m.experimental.fold(Nil)(_ => tableRow("Experimental", Seq("true")))

  def typeParams(m: Member): Seq[AppliedTag] = m.docs.fold(Nil)(d => flattenedDocPart(d.typeParams))
  def valueParams(m: Member): Seq[AppliedTag] = m.docs.fold(Nil)(d => flattenedDocPart(d.valueParams))

  def processLocalLinkWithGuard(str: String): String =
  if str.startsWith("#") || str.isEmpty then
    str
  else
    validationLink(str)

  def validationLink(str: String): String =
    def asValidURL = Try(URL(str)).toOption.map(_ => str)

    def asAsset: Option[String] = Option.when(
      Files.exists(Paths.get("docs/_assets").resolve(str))
      )(
        s"docs/_assets/$str"
        )

    asValidURL
      .orElse(asAsset)
      .getOrElse{
        report.warning(s"Unable to resolve link '$str'")
        str
      }

  def memberInfo(m: Member, withBrief: Boolean = false, full: Boolean = false): Seq[AppliedTag] =
    val comment = m.docs
    val bodyContents = m.docs.fold(Nil)(e => renderDocPart(e.body) :: Nil)

    val classLikeInfo: TagArg = classLikeParts(m, full)

    val memberTypeParams = typeParams(m)
    val memberValueParams = valueParams(m)
    val attributes = Seq(
      docAttributes(m),
      companion(m),
      deprecation(m),
      experimental(m),
      defintionClasses(m),
      inheritedFrom(m),
      source(m),
      classLikeInfo
    ).filter {
      case Nil => false
      case _ => true
    }

    val document = Jsoup.parse(bodyContents.mkString)
    val document2 = Jsoup.parse(attributes.mkString)

    document.select("img").forEach(element =>
      element.attr("src", validationLink(element.attr("src")))
    )

    Seq(
      Option.when(withBrief && comment.flatMap(_.short).nonEmpty)(div(cls := "documentableBrief doc")(comment.flatMap(_.short).fold("")(renderDocPart))),
      Option.when(bodyContents.nonEmpty || attributes.nonEmpty)(
        div(cls := "cover")(
          div(cls := "doc")(bodyContents),
          Option.when(full)(
            section(id := "attributes")(
              Option.when(memberTypeParams.nonEmpty)(Seq(
                h2(cls := "h500")("Type parameters"),
                dl(cls := "attributes")(memberTypeParams*)
              )).toSeq.flatten,
              Option.when(memberValueParams.nonEmpty)(Seq(
                h2(cls := "h500")("Value parameters"),
                dl(cls := "attributes")(memberValueParams*)
              )).toSeq.flatten,
              h2(cls := "h500")("Attributes"),
              dl(cls := "attributes")(attributes*)
            )
          ).getOrElse(
            Option.when(memberTypeParams.nonEmpty)(Seq(
              h2(cls := "h200")("Type parameters"),
              dl(cls := "attributes attributes-small")(memberTypeParams *)
            )).toSeq.flatten ++
            Option.when(memberValueParams.nonEmpty)(Seq(
              h2(cls := "h200")("Value parameters"),
              dl(cls := "attributes attributes-small")(memberValueParams *)
            )).toSeq.flatten :+
            h2(cls := "h200")("Attributes") :+
            dl(cls := "attributes attributes-small")(attributes *)
          )
        )
      )
    ).flatten

  private def originInfo(m: Member): Seq[TagArg] = m.origin match {
    case Origin.ImplicitlyAddedBy(name, dri) =>
      Seq("Implicitly added by ", renderLink(name, dri))
    case Origin.ExtensionFrom(name, dri) =>
      Seq("Extension method from ", renderLink(name, dri))
    case Origin.ExportedFrom(Some(link)) =>
      val signatureName: TagArg = link match
        case Link(name, dri) => renderLink(name, dri)
      Seq("Exported from ", signatureName)
    case _ => Nil
  }

  def memberSignature(member: Member) =
    val depStyle = if member.deprecated.isEmpty then "" else "deprecated"
    val nameClasses = Seq(
      cls := s"documentableName $depStyle",
    )

    val signature: MemberSignature = signatureProvider.rawSignature(member)()
    val isSubtype = signature.suffix.exists {
      case Keyword(keyword) => keyword.contains("extends")
      case _ => false
    }
    if !isSubtype then
      Seq(
        div(cls := "signature")(
          (Seq[TagArg](
            span(cls := "modifiers")(signature.prefix.map(renderElement(_))),
            span(cls := "kind")(signature.kind.map(renderElement(_))),
            signature.name.map(renderElement(_, nameClasses*))
          ) ++ signature.suffix.map(renderElement(_)))*
        ),
      )
    else
      val (beforeExtends, afterExtends) = signature.suffix.splitAt(signature.suffix.indexOf(Keyword("extends")))
      val (shortSuffix, longSuffix) = splitTypeSuffixSignature(beforeExtends, afterExtends)
      Seq(
        div(cls := "signature")(
          span(cls := "signature-short")(
            (Seq[TagArg](
              span(cls := "modifiers")(signature.prefix.map(renderElement(_))),
              span(cls := "kind")(signature.kind.map(renderElement(_))),
              signature.name.map(renderElement(_, nameClasses *))
            ) ++ shortSuffix.map(renderElement(_)))*
          ),
          span(cls := "signature-long")(
            longSuffix.map(renderElement(_))*
          )
        ),
      )
  end memberSignature

  def splitTypeSuffixSignature(shortAcc: List[SignaturePart], tail: List[SignaturePart], nestedTypeLevel: Int = 0): (List[SignaturePart], List[SignaturePart]) =
    tail match
      case Nil =>
        (shortAcc, Nil)
      case (head @ Plain("[")) :: rest =>
        splitTypeSuffixSignature(shortAcc :+ head, rest, nestedTypeLevel + 1)
      case (head @ Plain("]")) :: rest =>
        splitTypeSuffixSignature(shortAcc :+ head, rest, nestedTypeLevel - 1)
      case (head @ Keyword(", ")) :: rest if nestedTypeLevel == 0 =>
        (shortAcc :+ head, rest)
      case head :: rest =>
        splitTypeSuffixSignature(shortAcc :+ head, rest, nestedTypeLevel)


  def memberIcon(member: Member) = member.kind match {
    case _ =>

      // val iconSpan = span(cls := s"micon ${member.kind.name.take(2)}$withCompanion")()
      val iconSpan = span(cls := "icon")(
        span(cls := s"micon ${member.kind.name.take(2)}"),
        member.companion.map((kind, _) => span(cls := s"micon companion ${kind.name.take(2)}")).toList
      )
      Seq(member.companion.flatMap( (_, dri) => link(dri)).fold(iconSpan)(link => a(href := link)(iconSpan)))
  }

  def annotations(member: Member): Option[TagArg] =
   val rawBuilder = SignatureBuilder().annotationsBlock(member)
   val signatures = rawBuilder.content
   val rendered = signatures.map(renderElement(_))
   Option.when(rendered.nonEmpty)(span(cls := "annotations monospace")(rendered))

  def member(member: Member) =
    val filterAttributes = FilterAttributes.attributesFor(member)
    val anchor = if member.dri.anchor.isEmpty then Nil else Seq(id := member.dri.anchor)
    def topLevelAttr = Seq(cls := "documentableElement")
      ++ anchor
      ++ filterAttributes.map{ case (n, v) => Attr(s"data-f-$n") := v }

    val originInf = originInfo(member)
    val memberInf = memberInfo(member, withBrief = true)
    val annots = annotations(member)

    div(topLevelAttr*)(
      div(cls := "documentableElement-expander")(
        Option.when(annots.nonEmpty || originInf.nonEmpty || memberInf.nonEmpty)(button(cls := "icon-button ar show-content")).toList,
        annots.map(div(_)).toList,
        div(cls := "header monospace mono-medium")(memberSignature(member)),
      ),
      Option.when(originInf.nonEmpty || memberInf.nonEmpty)(
        div(cls := "docs")(
          span(cls := "modifiers"), // just to have padding on left
          div(
            div(cls := "originInfo")(originInf*),
            div(cls := "memberDocumentation")(memberInf*),
          )
        )
      ).toList
    )

  private case class MGroup(header: AppliedTag, members: Seq[Member], groupName: String)

  private def makeSubgroupHeader(name: String): AppliedTag =
    h4(cls := "groupHeader h200")(name)

  private def actualGroup(name: String, members: Seq[Member | MGroup], headerConstructor: String => AppliedTag = makeSubgroupHeader, wrapInSection: Boolean = true): Seq[AppliedTag] =
    if members.isEmpty then Nil else
      val groupBody = div(cls := "documentableList expand")(
        div(cls := "documentableList-expander")(
          button(cls := "icon-button show-content expand"),
          headerConstructor(name)
        ),
        members.sortBy {
          case m: Member => m.name
          case MGroup(_, _, name) => name
        }.map {
          case element: Member =>
            member(element)
          case MGroup(header, members, _) =>
            div(
              header,
              members.map(member)
            )
        }
      )
      if wrapInSection then
        section(id := name.replace(' ', '-'))(
          groupBody
        ) :: Nil
      else
        groupBody :: Nil


  private def isDeprecated(m: Member | MGroup): Boolean = m match
    case m: Member => m.deprecated.nonEmpty
    case g: MGroup => g.members.exists(isDeprecated)

  private def isExperimental(m: Member | MGroup): Boolean = m match
    case m: Member => m.experimental.nonEmpty
    case g: MGroup => g.members.exists(isExperimental)

  private def isInherited(m: Member | MGroup): Boolean = m match
    case m: Member => m.inheritedFrom.nonEmpty
    case g: MGroup => g.members.exists(isInherited)

  private def isAbstract(m: Member | MGroup): Boolean = m match
    case m: Member => m.modifiers.exists(Set(Modifier.Abstract, Modifier.Deferred).contains)
    case g: MGroup => g.members.exists(isAbstract)

  private type SubGroup = (String, Seq[Member | MGroup])
  private def buildGroup(name: String, subgroups: Seq[SubGroup]): Tab =
    val all = subgroups.map { case (name, members) =>
      val (experimental, nonExperimental) = members.partition(isExperimental)
      val (allInherited, allDefined) = nonExperimental.partition(isInherited)
      val (depDefined, defined) = allDefined.partition(isDeprecated)
      val (depInherited, inherited) = allInherited.partition(isDeprecated)
      val (abstractInherited, concreteInherited) = inherited.partition(isAbstract)
      val normalizedName = name.toLowerCase
      val definedWithGroup = if Set("methods", "fields").contains(normalizedName) then
          val (abstr, concr) = defined.partition(isAbstract)
          Seq(
            actualGroup(s"Abstract ${normalizedName}", abstr),
            actualGroup(s"Concrete ${normalizedName}", concr)
          )
        else
          Seq(actualGroup(name, defined))

      definedWithGroup ++ List(
        actualGroup(s"Deprecated ${normalizedName}", depDefined),
        actualGroup(s"Inherited ${normalizedName}", concreteInherited),
        actualGroup(s"Inherited and Abstract ${normalizedName}", abstractInherited),
        actualGroup(s"Deprecated and Inherited ${normalizedName}", depInherited),
        actualGroup(name = s"Experimental ${normalizedName}", experimental)
      )
    }

    val children = all.flatten.flatten
    if children.isEmpty then emptyTab
    else Tab(
      name,
      name,
      Seq(
        div(cls := "member-group-header")(
          h3(tabAttr(name), cls := "h400")(name)
        )
      ) ++ children,
      "expand"
    )

  case class ExpandedGroup(name: AppliedTag, description: AppliedTag, prio: Int)

  val emptyTab = Tab("", "", Nil)

  def grouppedMember(m: Member, membersInGroups: Seq[Member]): Tab =
    if membersInGroups.isEmpty then emptyTab else
      val descriptions = m.docs.map(_.groupDesc).getOrElse(Map.empty)
      val names = m.docs.map(_.groupNames).getOrElse(Map.empty)
      val prios = m.docs.map(_.groupPrio).getOrElse(Map.empty)

      val rawGroups = membersInGroups.groupBy(_.docs.flatMap(_.group)).collect {
          case (Some(groupName), members) =>
            ExpandedGroup(
              names.get(groupName).fold(raw(groupName))(renderDocPart),
              descriptions.get(groupName).fold(raw(""))(renderDocPart),
              prios.getOrElse(groupName, 1000)
            ) -> members
        }
      val content = rawGroups.toSeq.sortBy(_._1.prio).flatMap {
        case (group, members) =>
          Seq(div(cls := "documentableList expand")(
            button(cls := "icon-button show-content expand"),
            h3(cls := "h200")(group.name),
            group.description,
            members.map(member)
          ))
      }
      Tab("Grouped members", "grouped_members",
        Seq(
          div(cls := "member-group-header")(
            h3(tabAttr("grouped_members"), cls := "h400")("Grouped members")
          )
        ) ++ content,
        "expand")

  def buildMembers(s: Member): AppliedTag =
    def partitionIntoGroups(members: Seq[Member]) =
      if summon[DocContext].args.includeGroups then
        members.partition(_.docs.exists(_.group.nonEmpty))
      else
        (Nil, members)

    val (membersInGroups, rest) = partitionIntoGroups(s.members)

    val extensions =
      rest.groupBy{ _.kind match
        case Kind.Extension(on, _) => Some(on)
        case _ => None
      }.collect {
        case (Some(on), members) =>
          val typeSig = SignatureBuilder()
            .keyword("extension ")
            .typeParamList(on.typeParams)
            .content
          val argsSig = SignatureBuilder()
            .functionTermParameters(on.argsLists)
            .content
          val sig = typeSig ++ argsSig
          MGroup(span(cls := "groupHeader")(sig.map(renderElement(_))), members.sortBy(_.name).toSeq, on.name) -> on.position
      }.toSeq.sortBy(_._2).map(_._1)

    div(cls := "membersList expand")(
    renderTabs(
      singleSelection = false,
      Tab("Packages", "packages",
        actualGroup("Packages", rest.filter(m => m.kind == Kind.Package), name => h3(cls := "groupHeader h400")(name), false),
        "expand"),
      grouppedMember(s, membersInGroups),
      buildGroup("Type members", Seq(
        ("Classlikes", rest.filter(m => m.kind.isInstanceOf[Classlike])),
        ("Types", rest.filter(_.kind.isInstanceOf[Kind.Type])),
        ("Enum entries", rest.filter(_.kind.isInstanceOf[Kind.EnumCase])),
      )),
      buildGroup("Value members", Seq(
        ("Constructors", rest.filter(_.kind.isInstanceOf[Kind.Constructor])),
        ("Methods", rest.filter(_.kind.isInstanceOf[Kind.Def])),
        ("Fields", rest.filter(m => m.kind == Kind.Val || m.kind == Kind.Var)),
      )),
      buildGroup("Givens", Seq(
        ("Givens", rest.filter(_.kind.isInstanceOf[Kind.Given])),
      )),
      buildGroup("Extensions", Seq(
        ("Extensions", extensions),
      )),
      buildGroup("Implicits", Seq(
        ("Implicits", rest.filter(_.kind.isInstanceOf[Kind.Implicit])),
      )),
      buildGroup("Exports", Seq(
        ("Defined exports", rest.filter(_.kind.isInstanceOf[Kind.Exported])),
      ))
    ))

  case class Tab(name: String, id: String, content: Seq[AppliedTag], cls: String = "")

  def tabAttr(id: String) = Attr("data-togglable") := id

  private def renderTabs(singleSelection: Boolean, allTabs: Tab*): Seq[AppliedTag] =
    val tabs = allTabs.filter(_.content.nonEmpty)
      if tabs.isEmpty then Nil else
        Seq(div(cls := (if singleSelection then "tabs single" else "tabs"))(
            div(cls := "contents")(tabs.map(t =>
              section(id := t.name.replace(' ', '-'))(
                div(tabAttr(t.id), cls := s"tab ${t.cls}")(t.content)
              )
            ))
        ))

  def classLikeParts(m: Member, full: Boolean = true): TagArg =
    if !m.kind.isInstanceOf[Classlike] then Nil else
      val graphHtml = m.graph match
        case graph if graph.edges.nonEmpty && full =>
          Seq(div( id := "inheritance-diagram", cls := "diagram-class showGraph")(
            button(`type` := "button", cls := "label-only-button", onclick := "zoomOut()")("Reset zoom"),
            button(`type` := "button", cls := "label-only-button", onclick := "hideGraph()")("Hide graph"),
            button(`type` := "button", cls := "label-only-button", onclick := "showGraph()")("Show graph"),
            svg(id := "graph"),
            script(`type` := "text/dot", id := "dot")(
              raw(DotDiagramBuilder.build(graph, signatureRenderer))
            ),
          ))
        case _ => Nil

      def signatureList(list: Seq[LinkToType], className: String = "", expandable: Boolean): Seq[AppliedTag] =
        if list.isEmpty then Nil
         else Seq(div(cls := s"mono-small-block $className")(
         list.map(link =>
          div(link.kind.name," ", link.signature.map(renderElement(_)))),
          if(expandable) then span(cls := "show-all-code show-content text-button h100")("Show all") else span()
        ))

      def selfTypeList(list: List[LinkToType]): Seq[AppliedTag] =
        if list.isEmpty then Nil
        else Seq(
          div(cls := "mono-small-block supertypes")(
            span(),
            list.map { link =>
              div(link.signature.map(renderElement(_)))
            }
        ))

      val supertypes = signatureList(m.parents, "supertypes", m.parents.length > 5)
      val subtypes = signatureList(m.knownChildren, "subtypes", m.knownChildren.length > 5)
      val selfType = selfTypeList(m.selfType.toList)

      Seq(
        "Graph" -> graphHtml,
        "Supertypes" -> supertypes,
        "Known subtypes" -> subtypes,
        "Self type" -> selfType,
      ).filterNot(_._2.isEmpty).flatMap(tableRow(_, _))

  private def renderTable(keyValues: (TagArg, TagArg)*): TagArg =
    table(
      keyValues.map((key, value) => tr(td(key), td(value)))
    )

  private def buildDocumentableFilter = div(cls := "documentableFilter")(
    div(cls := "filtersContainer")(),
    input(cls := "filterableInput", placeholder := "Filter by any phrase", testId := "filterBarInput", `type` := "search"),
    button(cls := "clearButton label-only-button",  testId := "filterBarClearButton")("Clear all"),
  )

  private def companionBadge(m: Member): Seq[AppliedTag] = m.companion.fold(Nil) { companion =>
    Seq(div(cls := "companion-badge body-small")(
      span(
        "See the",
        span(cls := s"micon ${companion._1.name.take(2)}"),
        a(href := link(companion._2).getOrElse(""))(m.name),
        " companion ",
        companion._1.name
      )
    ))
  }

  def fullMember(m: Member): PageContent =
    val wideClass = m.companion.map(_ => "multi").getOrElse("single")
    val intro = m.kind match
      case Kind.RootPackage =>Seq(h1(cls := s"h600")(summon[DocContext].args.name))
      case _ =>
        Seq(
          div(cls := "cover-header")(
            memberIcon(m),
            h1(cls := s"h600 $wideClass")(m.name)
          ),
          div(cls := "fqname body-large")(
            span(m.fullName)
          )
        ) ++ companionBadge(m) ++
        Seq(
          div(cls := "main-signature mono-small-block")(
            annotations(m).getOrElse(Nil),
            memberSignature(m)
          )
        )

    val memberContent = div(
      intro,
      memberInfo(m, full = true),
      if m.members.length > 0 then
        Seq(section(id := "members-list")(
          h2(cls := "h500")("Members list"),
          buildDocumentableFilter,
          buildMembers(m)
        ))
      else Nil
    )

    val memberDocument = Jsoup.parse(memberContent.toString)

    val toc = memberDocument.select("section[id]").asScala.toSeq
      .flatMap { elem =>
        val header = elem.selectFirst("h1, h2, h3, h4, h5, h6")
        Option(header).map { h =>
          TocEntry(h.tag().getName, h.text(), s"#${elem.id()}")
        }
      }

    PageContent(
      memberContent,
      toc
    )
