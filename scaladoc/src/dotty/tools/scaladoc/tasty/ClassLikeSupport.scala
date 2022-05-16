package dotty.tools.scaladoc.tasty

import scala.jdk.CollectionConverters._
import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}
import dotty.tools.scaladoc.Inkuire

import scala.quoted._

import SymOps._
import NameNormalizer._
import SyntheticsSupport._
import dotty.tools.dotc.core.NameKinds

// Please use this only for things defined in the api.scala file
import dotty.tools.{scaladoc => api}

trait ClassLikeSupport:
  self: TastyParser =>
  import qctx.reflect._

  private given qctx.type = qctx

  private def bareClasslikeKind(using Quotes)(symbol: reflect.Symbol): Kind =
    import reflect._
    if symbol.flags.is(Flags.Module) then Kind.Object
    else if symbol.flags.is(Flags.Trait) then  Kind.Trait(Nil, Nil)
    else if symbol.flags.is(Flags.Enum) then Kind.Enum(Nil, Nil)
    else if symbol.flags.is(Flags.Enum) && symbol.flags.is(Flags.Case) then Kind.EnumCase(Kind.Object)
    else Kind.Class(Nil, Nil)

  private def kindForClasslike(classDef: ClassDef): Kind =
    def typeArgs = classDef.getTypeParams.map(mkTypeArgument(_, classDef))

    def parameterModifier(parameter: Symbol): String =
      val fieldSymbol = classDef.symbol.declaredField(parameter.normalizedName)
      def isVal = fieldSymbol.flags.is(Flags.ParamAccessor) &&
        !classDef.symbol.flags.is(Flags.Case) &&
        !fieldSymbol.flags.is(Flags.Private)

      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if isVal then "val "
      else ""

    def args = if constructorWithoutParamLists(classDef) then Nil else
      val constr =
        Some(classDef.constructor.symbol)
          .filter(s => s.exists && !s.isHiddenByVisibility)
          .map( _.tree.asInstanceOf[DefDef])

      constr.fold(Nil)(_.termParamss.map(pList =>
        api.TermParameterList(
          pList.params.map(p => mkParameter(p, classDef, parameterModifier)),
          paramListModifier(pList.params),
        )
      ))

    if classDef.symbol.flags.is(Flags.Module) then Kind.Object
    else if classDef.symbol.flags.is(Flags.Trait) then
      Kind.Trait(typeArgs, args)
    else if classDef.symbol.flags.is(Flags.Enum) && classDef.symbol.flags.is(Flags.Case) then Kind.EnumCase(Kind.Class(typeArgs, args))
    else if classDef.symbol.flags.is(Flags.Enum) then Kind.Enum(typeArgs, args)
    else Kind.Class(typeArgs, args)

  def mkClass(classDef: ClassDef)(
    dri: DRI = classDef.symbol.dri,
    name: String = classDef.symbol.normalizedName,
    signatureOnly: Boolean = false,
    modifiers: Seq[Modifier] = classDef.symbol.getExtraModifiers(),
  ): Member =
    def unpackTreeToClassDef(tree: Tree): ClassDef =
      def unpackApply(a: Apply) =
        a.symbol.owner.tree match
          case tree: ClassDef => tree

      tree match
        case tree: ClassDef => tree
        case TypeDef(_, tbt: TypeBoundsTree) => unpackTreeToClassDef(tbt.tpe.typeSymbol.tree)
        case TypeDef(_, tt: TypeTree) => unpackTreeToClassDef(tt.tpe.typeSymbol.tree)
        case c: Apply => unpackApply(c)
        case Block(_, c: Apply) => unpackApply(c)
        case tt: TypeTree => unpackTreeToClassDef(tt.tpe.typeSymbol.tree)

    def signatureWithName(s: dotty.tools.scaladoc.Signature): dotty.tools.scaladoc.Signature =
      s match
          case dotty.tools.scaladoc.Type(n, Some(dri)) :: tail => dotty.tools.scaladoc.Name(n, dri) :: tail
          case other => other

    def getSupertypesGraph(link: LinkToType, to: Seq[Tree]): Seq[(LinkToType, LinkToType)] =
      to.flatMap { case tree =>
        val symbol = if tree.symbol.isClassConstructor then tree.symbol.owner else tree.symbol
        val signature = signatureWithName(tree.asSignature(classDef))
        val superLink = LinkToType(signature, symbol.dri, bareClasslikeKind(symbol))
        val nextTo = unpackTreeToClassDef(tree).parents
        if symbol.isHiddenByVisibility then getSupertypesGraph(link, nextTo)
        else Seq(link -> superLink) ++ getSupertypesGraph(superLink, nextTo)
      }

    val supertypes = getSupertypes(using qctx)(classDef)
      .filterNot((s, t) => s.isHiddenByVisibility)
      .map {
        case (symbol, tpe) =>
          val signature = signatureWithName(tpe.asSignature(classDef))
          LinkToType(signature, symbol.dri, bareClasslikeKind(symbol))
      }
    val selfType = classDef.self.map { (valdef: ValDef) =>
      val symbol = valdef.symbol
      val tpe = valdef.tpt.tpe
      val signature = signatureWithName(tpe.asSignature(classDef))
      LinkToType(signature, symbol.dri, Kind.Type(false, false, Seq.empty))
    }
    val selfSignature: DSignature = signatureWithName(typeForClass(classDef).asSignature(classDef))

    val graph = HierarchyGraph.withEdges(
      getSupertypesGraph(LinkToType(selfSignature, classDef.symbol.dri, bareClasslikeKind(classDef.symbol)), unpackTreeToClassDef(classDef).parents)
    )

    val kind = if intrinsicClassDefs.contains(classDef.symbol) then Kind.Class(Nil, Nil) else kindForClasslike(classDef)

    val baseMember = mkMember(classDef.symbol, kind, selfSignature)(
      modifiers = modifiers,
      graph = graph,
      deprecated = classDef.symbol.isDeprecated(),
      experimental = classDef.symbol.isExperimental()
    ).copy(
      directParents = classDef.getParentsAsLinkToTypes,
      parents = supertypes,
    )

    if summon[DocContext].args.generateInkuire then doInkuireStuff(classDef)

    if signatureOnly then baseMember else baseMember.copy(
        members = classDef.extractPatchedMembers.sortBy(m => (m.name, m.kind.name)),
        selfType = selfType,
        companion = classDef.getCompanion
    )

  private val conversionSymbol = Symbol.requiredClass("scala.Conversion")

  def extractImplicitConversion(tpe: TypeRepr): Option[ImplicitConversion] =
      if tpe.derivesFrom(conversionSymbol) then tpe.baseType(conversionSymbol) match
        case AppliedType(tpe, List(from: TypeRepr, to: TypeRepr)) =>
          Some(ImplicitConversion(from.typeSymbol.dri, to.typeSymbol.dri))
        case _ =>
          None
      else None

  private def isDocumentableExtension(s: Symbol) =
    !s.isHiddenByVisibility && !s.isSyntheticFunc && s.isExtensionMethod

  private def parseMember(c: ClassDef)(s: Tree): Option[Member] = processTreeOpt(s) { s match
      case dd: DefDef if isDocumentableExtension(dd.symbol) =>
        dd.symbol.extendedSymbol.map { extSym =>
          val memberInfo = unwrapMemberInfo(c, dd.symbol)
          val typeParams = dd.symbol.extendedTypeParams.map(mkTypeArgument(_, c, memberInfo.genericTypes))
          val termParams = dd.symbol.extendedTermParamLists.zipWithIndex.flatMap { case (termParamList, index) =>
            memberInfo.termParamLists(index) match
              case MemberInfo.EvidenceOnlyParameterList => None
              case MemberInfo.RegularParameterList(info) =>
                Some(api.TermParameterList(termParamList.params.map(mkParameter(_, c, memberInfo = info)), paramListModifier(termParamList.params)))
              case _ => assert(false, "memberInfo.termParamLists contains a type parameter list !")
          }
          val target = ExtensionTarget(
            extSym.symbol.normalizedName,
            typeParams,
            termParams,
            extSym.tpt.asSignature(c),
            extSym.tpt.symbol.dri,
            extSym.symbol.pos.get.start
          )
          parseMethod(c, dd.symbol,specificKind = Kind.Extension(target, _))
        }

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isExported && !dd.symbol.isArtifact =>
        dd.rhs.map {
          case TypeApply(rhs, _) => rhs
          case Apply(TypeApply(rhs, _), _) => rhs
          case rhs => rhs
        }.map(_.tpe.termSymbol).filter(_.exists).map(_.tree).map {
          case v: ValDef if v.symbol.flags.is(Flags.Module) && !v.symbol.flags.is(Flags.Synthetic) =>
            v.symbol.owner -> Symbol.newVal(v.symbol.owner, dd.name, v.tpt.tpe, Flags.Final, Symbol.noSymbol).tree
          case other => other.symbol.owner -> other
        }.flatMap { (originalOwner, tree) =>
          parseMember(originalOwner.tree.asInstanceOf[ClassDef])(tree)
            .map { m => m
              .withDRI(dd.symbol.dri)
              .withName(dd.symbol.normalizedName)
              .withKind(Kind.Exported(m.kind))
              .withOrigin(Origin.ExportedFrom(Some(Link(originalOwner.normalizedName, originalOwner.dri))))
            }
        }

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isSyntheticFunc && !dd.symbol.isExtensionMethod && !dd.symbol.isArtifact =>
        Some(parseMethod(c, dd.symbol))

      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum)) =>
        Some(parseTypeDef(td, c))

      case vd: ValDef if !isSyntheticField(vd.symbol) && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum)) =>
        Some(parseValDef(c, vd))

      case c: ClassDef if c.symbol.shouldDocumentClasslike =>
        Some(parseClasslike(c))

      case _ => None
  }

  private def parseInheritedMember(c: ClassDef)(s: Tree): Option[Member] =
    def inheritance = Some(InheritedFrom(s.symbol.owner.normalizedName, s.symbol.dri, s.symbol.owner.isHiddenByVisibility))
    processTreeOpt(s)(s match
      case c: ClassDef if c.symbol.shouldDocumentClasslike => Some(parseClasslike(c, signatureOnly = true))
      case other => {
        val parsed = parseMember(c)(other)
        parsed.map(p =>
          val parentDRI = c.symbol.dri
          p.copy(
            dri = p.dri.copy(
              location = parentDRI.location,
              externalLink = None
            )
          )
        )
      }
    ).map(_.copy(inheritedFrom = inheritance))

  extension (using Quotes)(c: reflect.ClassDef)

    def membersToDocument = c.body.filterNot(_.symbol.isHiddenByVisibility)

    def getNonTrivialInheritedMemberTrees =
      c.symbol.getmembers.filterNot(s => s.isHiddenByVisibility || s.maybeOwner == c.symbol)
        .filter(s => s.maybeOwner != defn.ObjectClass && s.maybeOwner != defn.AnyClass)
        .map(_.tree)

  extension (c: ClassDef)
    def extractMembers: Seq[Member] = {
      val inherited = c.getNonTrivialInheritedMemberTrees.collect {
        case dd: DefDef if !dd.symbol.isClassConstructor && !(dd.symbol.isSuperBridgeMethod || dd.symbol.isDefaultHelperMethod) => dd
        case other => other
      }
      c.membersToDocument.flatMap(parseMember(c)) ++
        inherited.flatMap(s => parseInheritedMember(c)(s))
    }

    /** Extracts members while taking Dotty logic for patching the stdlib into account. */
    def extractPatchedMembers: Seq[Member] = {
      val ownMembers = c.extractMembers
      def extractPatchMembers(sym: Symbol) = {
        // NOTE for some reason scala.language$.experimental$ class doesn't show up here, so we manually add the name
        val ownMemberDRIs = ownMembers.iterator.map(_.name).toSet + "experimental$"
        sym.tree.asInstanceOf[ClassDef]
          .membersToDocument.filterNot(m => ownMemberDRIs.contains(m.symbol.name))
          .flatMap(parseMember(c))
      }
      c.symbol.fullName match {
        case "scala.Predef$" =>
          ownMembers ++
          extractPatchMembers(qctx.reflect.Symbol.requiredClass("scala.runtime.stdLibPatches.Predef$"))
        case "scala.language$" =>
          ownMembers ++
          extractPatchMembers(qctx.reflect.Symbol.requiredModule("scala.runtime.stdLibPatches.language").moduleClass)
        case "scala.language$.experimental$" =>
          ownMembers ++
          extractPatchMembers(qctx.reflect.Symbol.requiredModule("scala.runtime.stdLibPatches.language.experimental").moduleClass)
        case _ => ownMembers
      }

    }

    def getTreeOfFirstParent: Option[Tree] =
      c.getParentsAsTreeSymbolTuples.headOption.map(_._1)

    def getParentsAsLinkToTypes: List[LinkToType] =
      c.getParentsAsTreeSymbolTuples.map {
        (tree, symbol) => LinkToType(tree.asSignature(c), symbol.dri, bareClasslikeKind(symbol))
      }

    def getParentsAsTreeSymbolTuples: List[(Tree, Symbol)] =
      if noPosClassDefs.contains(c.symbol) then Nil
      else for
        // TODO: add exists function to position methods in Quotes and replace the condition here for checking the JPath
        parentTree <- c.parents if parentTree.pos.sourceFile.getJPath.isDefined && parentTree.pos.start != parentTree.pos.end // We assume here that order is correct
        parentSymbol = parentTree match
          case t: TypeTree => t.tpe.typeSymbol
          case tree if tree.symbol.isClassConstructor => tree.symbol.owner
          case tree => tree.symbol
        if parentSymbol != defn.ObjectClass && parentSymbol != defn.AnyClass && !parentSymbol.isHiddenByVisibility
      yield (parentTree, parentSymbol)

    def getConstructors: List[Symbol] = c.membersToDocument.collect {
      case d: DefDef if d.symbol.isClassConstructor && c.constructor.symbol != d.symbol => d.symbol
    }.toList

    def getParameterModifier(parameter: Symbol): String =
      val fieldSymbol = c.symbol.declaredField(parameter.normalizedName)
      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if fieldSymbol.flags.is(Flags.ParamAccessor) && !c.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
      else ""

    def getTypeParams: List[TypeDef] =
      c.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

    def getCompanion: Option[(Kind, DRI)] = c.symbol.getCompanionSymbol
      .filter(!_.flags.is(Flags.Synthetic))
      .filterNot(_.isHiddenByVisibility)
      .map(s => (bareClasslikeKind(s), s.dri))


  def parseClasslike(classDef: ClassDef, signatureOnly: Boolean = false): Member = classDef match
    case c: ClassDef if classDef.symbol.flags.is(Flags.Module) => parseObject(c, signatureOnly)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) && !classDef.symbol.flags.is(Flags.Case) => parseEnum(c, signatureOnly)
    case clazz => mkClass(classDef)(signatureOnly = signatureOnly)

  def parseObject(classDef: ClassDef, signatureOnly: Boolean = false): Member =
    mkClass(classDef)(
      // All objects are final so we do not need final modifier!
      modifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Final),
      signatureOnly = signatureOnly
    )

  def parseEnum(classDef: ClassDef, signatureOnly: Boolean = false): Member =
    val extraModifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Sealed).filter(_ != Modifier.Abstract)
    val companion = classDef.symbol.getCompanionSymbol.map(_.tree.asInstanceOf[ClassDef]).get

    val enumVals = companion.membersToDocument.collect {
      case vd: ValDef if !isSyntheticField(vd.symbol) && vd.symbol.flags.is(Flags.Enum) && vd.symbol.flags.is(Flags.Case) => vd
    }.toList.map(parseValDef(companion, _))

    val enumTypes = companion.membersToDocument.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef(_, classDef))

    val enumNested = companion.membersToDocument.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten

    val enumClass = mkClass(classDef)(modifiers = extraModifiers, signatureOnly = signatureOnly)

    val cases = (
      enumNested ++
      enumTypes ++
      enumVals.map(m => m.copy(dri = m.dri.copy(location = enumClass.dri.location)))
    )

    enumClass.withMembers(cases)

  def parseMethod(
      c: ClassDef,
      methodSymbol: Symbol,
      paramPrefix: Symbol => String = _ => "",
      specificKind: (Kind.Def => Kind) = identity
    ): Member =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = methodSymbol.nonExtensionParamLists

    val memberInfo = unwrapMemberInfo(c, methodSymbol)

    val unshuffledMemberInfoParamLists =
      if methodSymbol.isExtensionMethod && methodSymbol.isRightAssoc then
        // Taken from RefinedPrinter.scala
        // If you change the names of the clauses below, also change them in right-associative-extension-methods.md
        val (leftTyParams, rest1) = memberInfo.paramLists.span(_.isType)
        val (leadingUsing, rest2) = rest1.span(_.isUsing)
        val (rightTyParams, rest3) = rest2.span(_.isType)
        val (rightParam, rest4) = rest3.splitAt(1)
        val (leftParam, rest5) = rest4.splitAt(1)
        val (trailingUsing, rest6) = rest5.span(_.isUsing)
        if leftParam.nonEmpty then
          // leftTyParams ::: leadingUsing ::: leftParam ::: trailingUsing ::: rightTyParams ::: rightParam ::: rest6
          // because of takeRight after, this is equivalent to the following:
          rightTyParams ::: rightParam ::: rest6
        else
          memberInfo.paramLists // it wasn't a binary operator, after all.
      else
        memberInfo.paramLists

    val croppedUnshuffledMemberInfoParamLists = unshuffledMemberInfoParamLists.takeRight(paramLists.length)

    val basicDefKind: Kind.Def = Kind.Def(
      paramLists.zip(croppedUnshuffledMemberInfoParamLists).flatMap{
        case (_: TermParamClause, MemberInfo.EvidenceOnlyParameterList) => Nil
        case (pList: TermParamClause, MemberInfo.RegularParameterList(info)) =>
            Some(Left(api.TermParameterList(pList.params.map(
              mkParameter(_, c, paramPrefix, memberInfo = info)), paramListModifier(pList.params)
            )))
        case (TypeParamClause(genericTypeList), MemberInfo.TypeParameterList(memInfoTypes)) =>
          Some(Right(genericTypeList.map(mkTypeArgument(_, c, memInfoTypes, memberInfo.contextBounds))))
        case (_,_) =>
          assert(false, s"croppedUnshuffledMemberInfoParamLists and SymOps.nonExtensionParamLists disagree on whether this clause is a type or term one")
      }
    )

    val methodKind =
      if methodSymbol.isClassConstructor then Kind.Constructor(basicDefKind)
      else if methodSymbol.flags.is(Flags.Implicit) then
        val termParamLists: List[TermParamClause] = methodSymbol.nonExtensionTermParamLists
        extractImplicitConversion(method.returnTpt.tpe) match
          case Some(conversion) if termParamLists.size == 0 || (termParamLists.size == 1 && termParamLists.head.params.size == 0) =>
            Kind.Implicit(basicDefKind, Some(conversion))
          case None if termParamLists.size == 1 && termParamLists(0).params.size == 1 =>
            Kind.Implicit(basicDefKind, Some(
              ImplicitConversion(
                termParamLists(0).params(0).tpt.tpe.typeSymbol.dri,
                method.returnTpt.tpe.typeSymbol.dri
              )
            ))
          case _ =>
            Kind.Implicit(basicDefKind, None)
      else if methodSymbol.flags.is(Flags.Given) then Kind.Given(basicDefKind, Some(method.returnTpt.tpe.asSignature(c)), extractImplicitConversion(method.returnTpt.tpe))
      else specificKind(basicDefKind)

    val origin = if !methodSymbol.isOverridden then Origin.RegularlyDefined else
      val overriddenSyms = methodSymbol.allOverriddenSymbols.map(_.owner)
      Origin.Overrides(overriddenSyms.map(s => Overridden(s.name, s.dri)).toSeq)

    val modifiers = methodKind match
      case _: Kind.Given => methodSymbol
        .getExtraModifiers()
        .filterNot(m => m == Modifier.Lazy || m == Modifier.Final)
      case _ => methodSymbol.getExtraModifiers()

    mkMember(
      methodSymbol,
      methodKind,
      method.returnTpt.tpe.asSignature(c),
    )(
      modifiers = modifiers,
      origin = origin,
      deprecated = methodSymbol.isDeprecated(),
      experimental = methodSymbol.isExperimental()
    )

  def mkParameter(
    argument: ValDef,
    classDef: ClassDef,
    prefix: Symbol => String = _ => "",
    isExtendedSymbol: Boolean = false,
    isGrouped: Boolean = false,
    memberInfo: Map[String, TypeRepr] = Map.empty,
  ) =
    val inlinePrefix = if argument.symbol.flags.is(Flags.Inline) then "inline " else ""
    val nameIfNotSynthetic = Option.when(!argument.symbol.flags.is(Flags.Synthetic))(argument.symbol.normalizedName)
    val name = argument.symbol.normalizedName
    api.TermParameter(
      argument.symbol.getAnnotations(),
      inlinePrefix + prefix(argument.symbol),
      nameIfNotSynthetic,
      argument.symbol.dri,
      memberInfo.get(name).fold(argument.tpt.asSignature(classDef))(_.asSignature(classDef)),
      isExtendedSymbol,
      isGrouped
    )

  def mkTypeArgument(
    argument: TypeDef,
    classDef: ClassDef,
    memberInfo: Map[String, TypeBounds] = Map.empty,
    contextBounds: Map[String, DSignature] = Map.empty,
  ): TypeParameter =
    val variancePrefix: "+" | "-" | "" =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    val name = argument.symbol.normalizedName
    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    val boundsSignature = memberInfo.get(name).fold(argument.rhs.asSignature(classDef))(_.asSignature(classDef))
    val signature = contextBounds.get(name) match
      case None => boundsSignature
      case Some(contextBoundsSignature) =>
        boundsSignature ++ DSignature(Plain(" : ")) ++ contextBoundsSignature

    TypeParameter(
      argument.symbol.getAnnotations(),
      variancePrefix,
      normalizedName,
      argument.symbol.dri,
      signature
    )

  def parseTypeDef(typeDef: TypeDef, classDef: ClassDef): Member =
    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }
    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(mkTypeArgument(_, classDef)), body)
      case tpe => (Nil, tpe)

    val defaultKind = Kind.Type(!isTreeAbstract(typeDef.rhs), typeDef.symbol.isOpaque, generics).asInstanceOf[Kind.Type]
    val kind = if typeDef.symbol.flags.is(Flags.Enum) then Kind.EnumCase(defaultKind)
      else defaultKind

    if typeDef.symbol.flags.is(Flags.Exported)
    then {
      val origin = Some(tpeTree).flatMap {
        case TypeBoundsTree(l: TypeTree, h: TypeTree) if l.tpe == h.tpe =>
          Some(Link(l.tpe.typeSymbol.owner.name, l.tpe.typeSymbol.owner.dri))
        case _ => None
      }
      mkMember(typeDef.symbol, Kind.Exported(kind), tpeTree.asSignature(classDef))(
        deprecated = typeDef.symbol.isDeprecated(),
        origin = Origin.ExportedFrom(origin),
        experimental = typeDef.symbol.isExperimental()
      )
    }
    else mkMember(typeDef.symbol, kind, tpeTree.asSignature(classDef))(deprecated = typeDef.symbol.isDeprecated())

  def parseValDef(c: ClassDef, valDef: ValDef): Member =
    def defaultKind = if valDef.symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val memberInfo = unwrapMemberInfo(c, valDef.symbol)
    val kind = if valDef.symbol.flags.is(Flags.Implicit) then Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
      else if valDef.symbol.flags.is(Flags.Given) then Kind.Given(Kind.Val, Some(memberInfo.res.asSignature(c)), extractImplicitConversion(valDef.tpt.tpe))
      else if valDef.symbol.flags.is(Flags.Enum) then Kind.EnumCase(Kind.Val)
      else defaultKind

    val modifiers = kind match
      case _: Kind.Given => valDef.symbol
        .getExtraModifiers()
        .filterNot(m => m == Modifier.Lazy || m == Modifier.Final)
      case _ => valDef.symbol.getExtraModifiers()

    mkMember(valDef.symbol, kind, memberInfo.res.asSignature(c))(
      modifiers = modifiers,
      deprecated = valDef.symbol.isDeprecated(),
      experimental = valDef.symbol.isExperimental()
    )

  def mkMember(symbol: Symbol, kind: Kind, signature: DSignature)(
    modifiers: Seq[Modifier] = symbol.getExtraModifiers(),
    origin: Origin = Origin.RegularlyDefined,
    inheritedFrom: Option[InheritedFrom] = None,
    graph: HierarchyGraph = HierarchyGraph.empty,
    deprecated: Option[Annotation] = None,
    experimental: Option[Annotation] = None
  ) = Member(
    name = symbol.normalizedName,
    fullName = symbol.normalizedFullName,
    dri = symbol.dri,
    kind = kind,
    visibility = symbol.getVisibility(),
    modifiers = modifiers,
    annotations = symbol.getAnnotations(),
    signature = signature,
    sources = symbol.source,
    origin = origin,
    inheritedFrom = inheritedFrom,
    graph = graph,
    docs = symbol.documentation,
    deprecated = deprecated,
    experimental = experimental
  )


  case class MemberInfo(
    paramLists: List[MemberInfo.ParameterList],
    res: TypeRepr,
    contextBounds: Map[String, DSignature] = Map.empty,
  ){
    val genericTypes: Map[String, TypeBounds] = paramLists.collect{ case MemberInfo.TypeParameterList(types) => types }.headOption.getOrElse(Map())

    val termParamLists: List[MemberInfo.ParameterList] = paramLists.filter(_.isTerm)
  }

  object MemberInfo:
    enum ParameterList(val isTerm: Boolean, val isUsing: Boolean):
      inline def isType = !isTerm
      case EvidenceOnlyParameterList                                         extends ParameterList(isTerm = true, isUsing = false)
      case RegularParameterList(m: Map[String, TypeRepr])(isUsing: Boolean)  extends ParameterList(isTerm = true, isUsing)
      case TypeParameterList(m: Map[String, TypeBounds])                     extends ParameterList(isTerm = false, isUsing = false)

    export ParameterList.{RegularParameterList, EvidenceOnlyParameterList, TypeParameterList}



  def unwrapMemberInfo(c: ClassDef, symbol: Symbol): MemberInfo =
    val baseTypeRepr = typeForClass(c).memberType(symbol)

    def isSyntheticEvidence(name: String) =
      if !name.startsWith(NameKinds.ContextBoundParamName.separator) then false else
        // This assumes that every parameter that starts with `evidence$` and is implicit is generated by compiler to desugar context bound.
        // Howrever, this is just a heuristic, so
        // `def foo[A](evidence$1: ClassTag[A]) = 1`
        // will be documented as
        // `def foo[A: ClassTag] = 1`.
        // Scala spec states that `$` should not be used in names and behaviour may be undefiend in such case.
        // Documenting method slightly different then its definition is withing the 'undefiend behaviour'.
        symbol.paramSymss.flatten.find(_.name == name).exists(_.flags.is(Flags.Implicit))

    def handlePolyType(memberInfo: MemberInfo, polyType: PolyType): MemberInfo =
      val typeParamList = MemberInfo.TypeParameterList(polyType.paramNames.zip(polyType.paramBounds).toMap)
      MemberInfo(memberInfo.paramLists :+ typeParamList, polyType.resType)

    def handleMethodType(memberInfo: MemberInfo, methodType: MethodType): MemberInfo =
      val rawParams = methodType.paramNames.zip(methodType.paramTypes).toMap
      val isUsing = methodType.isImplicit
      val (evidences, notEvidences) = rawParams.partition(e => isSyntheticEvidence(e._1))

      def findParamRefs(t: TypeRepr): Seq[ParamRef] = t match
        case paramRef: ParamRef => Seq(paramRef)
        case AppliedType(_, args) => args.flatMap(findParamRefs)
        case MatchType(bound, scrutinee,  cases) =>
            findParamRefs(bound) ++ findParamRefs(scrutinee)
        case _ => Nil

      def nameForRef(ref: ParamRef): String =
        val PolyType(names, _, _) = ref.binder: @unchecked
        names(ref.paramNum)

      val (paramsThatLookLikeContextBounds, contextBounds) =
        evidences.partitionMap {
          case (_, AppliedType(tpe, List(typeParam: ParamRef))) =>
            Right(nameForRef(typeParam) -> tpe.asSignature(c))
          case (name, original) =>
            findParamRefs(original) match
              case Nil => Left((name, original))
              case typeParam :: _ =>
                val name = nameForRef(typeParam)
                val signature = Seq(
                  Plain("(["),
                  dotty.tools.scaladoc.Type(name, None),
                  Plain("]"),
                  Keyword(" =>> "),
                ) ++ original.asSignature(c) ++ Seq(Plain(")"))
                Right(name -> signature.toList)
        }

      val newParams = notEvidences ++ paramsThatLookLikeContextBounds

      val termParamList = if newParams.isEmpty && contextBounds.nonEmpty
        then MemberInfo.EvidenceOnlyParameterList
        else MemberInfo.RegularParameterList(newParams)(isUsing)


      MemberInfo(memberInfo.paramLists :+ termParamList, methodType.resType, contextBounds.toMap)

    def handleByNameType(memberInfo: MemberInfo, byNameType: ByNameType): MemberInfo =
      MemberInfo(memberInfo.paramLists, byNameType.underlying)

    def recursivelyCalculateMemberInfo(memberInfo: MemberInfo): MemberInfo = memberInfo.res match
      case p: PolyType => recursivelyCalculateMemberInfo(handlePolyType(memberInfo, p))
      case m: MethodType => recursivelyCalculateMemberInfo(handleMethodType(memberInfo, m))
      case b: ByNameType => handleByNameType(memberInfo, b)
      case _ => memberInfo

    recursivelyCalculateMemberInfo(MemberInfo(List.empty, baseTypeRepr))

  private def paramListModifier(parameters: Seq[ValDef]): String =
    if parameters.size > 0 then
      if parameters(0).symbol.flags.is(Flags.Given) then "using "
      else if parameters(0).symbol.flags.is(Flags.Implicit) then "implicit "
      else ""
    else ""
