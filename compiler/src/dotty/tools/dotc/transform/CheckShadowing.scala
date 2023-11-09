package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees.EmptyTree
import dotty.tools.dotc.transform.MegaPhase
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.util.{Property, SrcPos}
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.ast.tpd.TreeTraverser
import dotty.tools.dotc.core.Types.watchList
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.semanticdb.TypeOps
import dotty.tools.dotc.cc.boxedCaptureSet
import dotty.tools.dotc.core.Symbols.{NoSymbol, isParamOrAccessor}
import scala.collection.mutable
import dotty.tools.dotc.core.Scopes.Scope
import scala.collection.immutable.HashMap
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.ast.Trees.Ident
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.Mode.Type
import dotty.tools.dotc.core.Names.SimpleName

class CheckShadowing extends MiniPhase:
  import CheckShadowing.*
  import ShadowingData.*

  private val _key = Property.Key[ShadowingData]

  private def shadowingDataApply[U](f: ShadowingData => U)(using Context): Context =
    ctx.property(_key).foreach(f)
    ctx

  override def phaseName: String = CheckShadowing.name

  override def description: String = CheckShadowing.description

  override def isRunnable(using Context): Boolean =
    super.isRunnable &&
    ctx.settings.Xlint.value.nonEmpty &&
    !ctx.isJava &&
    !ctx.isOutlineFirstPass

  // Setup before the traversal
  override def prepareForUnit(tree: tpd.Tree)(using Context): Context =
    val data = ShadowingData()
    val fresh = ctx.fresh.setProperty(_key, data)
    shadowingDataApply(sd => sd.registerRootImports())(using fresh)

  // Reporting on traversal's end
  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree =
    shadowingDataApply(sd =>
      reportShadowing(sd.getShadowingResult)
    )
    tree

  // MiniPhase traversal :

  override def prepareForPackageDef(tree: tpd.PackageDef)(using Context): Context =
    shadowingDataApply(sd => sd.inNewScope())
    ctx

  override def prepareForTemplate(tree: tpd.Template)(using Context): Context =
    shadowingDataApply(sd => sd.inNewScope())
    ctx

  override def prepareForBlock(tree: tpd.Block)(using Context): Context =
    shadowingDataApply(sd => sd.inNewScope())
    ctx

  override def prepareForOther(tree: tpd.Tree)(using Context): Context =
    importTraverser.traverse(tree)
    ctx

  override def prepareForValDef(tree: tpd.ValDef)(using Context): Context =
    shadowingDataApply(sd =>
      sd.registerPrivateShadows(tree)
    )

  override def prepareForTypeDef(tree: tpd.TypeDef)(using Context): Context =
    if tree.symbol.isAliasType then // if alias, the parent is the current symbol
      nestedTypeTraverser(tree.symbol).traverse(tree.rhs)
    if tree.symbol.is(Param) then // if param, the parent is up
      val owner = tree.symbol.owner
      val parent = if (owner.isConstructor) then owner.owner else owner
      nestedTypeTraverser(parent).traverse(tree.rhs)(using ctx.outer)
      shadowingDataApply(sd => sd.registerCandidate(parent, tree))
    else
      ctx


  override def transformPackageDef(tree: tpd.PackageDef)(using Context): tpd.Tree =
    shadowingDataApply(sd => sd.outOfScope())
    tree

  override def transformBlock(tree: tpd.Block)(using Context): tpd.Tree =
    shadowingDataApply(sd => sd.outOfScope())
    tree

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    shadowingDataApply(sd => sd.outOfScope())
    tree

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
    if tree.symbol.is(Param) &&  isValidTypeParamOwner(tree.symbol.owner) then // Do not register for constructors the work is done for the Class owned equivalent TypeDef
      shadowingDataApply(sd => sd.computeTypeParamShadowsFor(tree.symbol.owner)(using ctx.outer))
    if tree.symbol.isAliasType then // No need to start outer here, because the TypeDef reached here it's already the parent
      shadowingDataApply(sd => sd.computeTypeParamShadowsFor(tree.symbol)(using ctx))
    tree

  // Helpers :
  private def isValidTypeParamOwner(owner: Symbol)(using Context): Boolean =
    !owner.isConstructor && !owner.is(Synthetic) && !owner.is(Exported)

  private def reportShadowing(res: ShadowingData.ShadowResult)(using Context): Unit =
    res.warnings.sortBy(w => (w.pos.line, w.pos.startPos.column))(using Ordering[(Int, Int)]).foreach {
      case PrivateShadowWarning(pos, shadow, shadowed) =>
        report.warning(s"${shadow.showLocated} shadows field ${shadowed.name} inherited from ${shadowed.owner}", pos)
      case TypeParamShadowWarning(pos, shadow, parent, shadowed) =>
        if shadowed.exists then
          report.warning(s"Type parameter ${shadow.name} for $parent shadows the type defined by ${shadowed.showLocated}", pos)
        else
          report.warning(s"Type parameter ${shadow.name} for $parent shadows an explicitly renamed type : ${shadow.name}", pos)
    }

  private def nestedTypeTraverser(parent: Symbol) = new TreeTraverser:
    import tpd.*

    override def traverse(tree: tpd.Tree)(using Context): Unit =
      tree match
        case t:tpd.TypeDef =>
          val newCtx = shadowingDataApply(sd =>
            sd.registerCandidate(parent, t)
          )
          traverseChildren(tree)(using newCtx)
        case _ =>
          traverseChildren(tree)
    end traverse
  end nestedTypeTraverser

  // To reach the imports during a miniphase traversal
  private def importTraverser = new TreeTraverser:
    import tpd.*

    override def traverse(tree: tpd.Tree)(using Context): Unit =
      tree match
        case t:tpd.Import =>
          val newCtx = shadowingDataApply(sd => sd.registerImport(t))
          traverseChildren(tree)(using newCtx)
        case _ =>
          traverseChildren(tree)

end CheckShadowing


object CheckShadowing:

  val name = "checkShadowing"
  val description = "check for elements shadowing other elements in scope"

  private class ShadowingData:
    import dotty.tools.dotc.transform.CheckShadowing.ShadowingData.*
    import collection.mutable.{Set => MutSet, Map => MutMap, Stack => MutStack}

    private val rootImports = MutSet[SingleDenotation]()
    private val explicitsImports = MutStack[MutSet[tpd.Import]]()
    private val renamedImports = MutStack[MutMap[SimpleName, Name]]() // original name -> renamed name

    private val typeParamCandidates = MutMap[Symbol, Seq[tpd.TypeDef]]().withDefaultValue(Seq())
    private val typeParamShadowWarnings = MutSet[TypeParamShadowWarning]()

    private val privateShadowWarnings = MutSet[PrivateShadowWarning]()

    def inNewScope()(using Context) =
      explicitsImports.push(MutSet())
      renamedImports.push(MutMap())

    def outOfScope()(using Context) =
      explicitsImports.pop()
      renamedImports.pop()

    /** Register the Root imports (at once per compilation unit)*/
    def registerRootImports()(using Context) =
      val langPackageName = ctx.definitions.JavaLangPackageVal.name.toSimpleName // excludes lang package
      rootImports.addAll(ctx.definitions.rootImportTypes.withFilter(_.name.toSimpleName != langPackageName).flatMap(_.typeMembers))

    /* Register an import encountered in the current scope **/
    def registerImport(imp: tpd.Import)(using Context) =
      val renamedImps = imp.selectors.collect(sel => { sel.renamed match
        case Ident(rename) =>
          (sel.name.toSimpleName, rename)
      }).toMap
      explicitsImports.top += imp
      renamedImports.top.addAll(renamedImps)

    /** Register a potential type definition which could shadows a Type already defined */
    def registerCandidate(parent: Symbol, typeDef: tpd.TypeDef) =
      val actual = typeParamCandidates.getOrElseUpdate(parent, Seq())
      typeParamCandidates.update(parent, actual.+:(typeDef))

    /** Compute if there is some TypeParam shadowing and register if it is the case */
    def computeTypeParamShadowsFor(parent: Symbol)(using Context): Unit =
        typeParamCandidates(parent).foreach(typeDef => {
          val sym = typeDef.symbol
          val shadowedType =
            lookForRootShadowedType(sym)
              .orElse(lookForImportedShadowedType(sym))
              .orElse(lookForUnitShadowedType(sym))
          shadowedType.foreach(shadowed =>
            if !renamedImports.exists(_.contains(shadowed.name.toSimpleName)) then
              typeParamShadowWarnings += TypeParamShadowWarning(typeDef.srcPos, typeDef.symbol, parent, shadowed)
          )
        })

    private def lookForRootShadowedType(symbol: Symbol)(using Context): Option[Symbol] =
      rootImports.find(p => p.name.toSimpleName == symbol.name.toSimpleName).map(_.symbol)

    private def lookForImportedShadowedType(symbol: Symbol)(using Context): Option[Symbol] =
      explicitsImports
        .flatMap(_.flatMap(imp => symbol.isAnImportedType(imp)))
        .headOption

    private def lookForUnitShadowedType(symbol: Symbol)(using Context): Option[Symbol] =
      if !ctx.owner.exists then
        None
      else
        val declarationScope = ctx.effectiveScope
        val res = declarationScope.lookup(symbol.name)
        res match
          case s: Symbol if s.isType => Some(s)
          case _ => lookForUnitShadowedType(symbol)(using ctx.outer)

    /** Register if the valDef is a private declaration that shadows an inherited field */
    def registerPrivateShadows(valDef: tpd.ValDef)(using Context): Unit =
      lookForShadowedField(valDef.symbol).foreach(shadowedField =>
        privateShadowWarnings += PrivateShadowWarning(valDef.startPos, valDef.symbol, shadowedField)
      )

    private def lookForShadowedField(symDecl: Symbol)(using Context): Option[Symbol] =
      if symDecl.isPrivate then
        val symDeclType = symDecl.info
        val bClasses = symDecl.owner.info.baseClasses
        bClasses match
          case _ :: inherited =>
            inherited
              .map(classSymbol => symDecl.denot.matchingDecl(classSymbol, symDeclType))
              .find(sym => sym.name == symDecl.name)
          case Nil =>
            None
      else
        None

    /** Get the shadowing analysis's result */
    def getShadowingResult(using Context): ShadowResult =
      val privateWarnings: List[ShadowWarning] =
        if ctx.settings.XlintHas.privateShadow then
          privateShadowWarnings.toList
        else
          Nil
      val typeParamWarnings: List[ShadowWarning] =
        if ctx.settings.XlintHas.typeParameterShadow then
          typeParamShadowWarnings.toList
        else
          Nil
      ShadowResult(privateWarnings ++ typeParamWarnings)

    extension (sym: Symbol)
      /** Looks after any type import symbol in the given import that matches this symbol */
      private def isAnImportedType(imp: tpd.Import)(using Context): Option[Symbol] =
        val tpd.Import(qual, sels) = imp
        val simpleSelections = qual.tpe.member(sym.name).alternatives
        val typeSelections = sels.flatMap(n => qual.tpe.member(n.name.toTypeName).alternatives)
        sels
          .find(is => is.rename.toSimpleName == sym.name.toSimpleName).map(_.symbol)
          .orElse(typeSelections.map(_.symbol).find(sd => sd.name == sym.name))
          .orElse(simpleSelections.map(_.symbol).find(sd => sd.name == sym.name))

  end ShadowingData

  private object ShadowingData:
      sealed abstract class ShadowWarning(val pos: SrcPos, val shadow: Symbol, val shadowed: Symbol)

      case class PrivateShadowWarning(
        override val pos: SrcPos,
        override val shadow: Symbol,
        override val shadowed: Symbol
      ) extends ShadowWarning(pos, shadow, shadowed)

      case class TypeParamShadowWarning(
        override val pos: SrcPos,
        override val shadow: Symbol,
        val shadowParent: Symbol,
        override val shadowed: Symbol,
      ) extends ShadowWarning(pos, shadow, shadowed)

      /** A container for the results of the shadow elements analysis */
      case class ShadowResult(warnings: List[ShadowWarning])

end CheckShadowing

