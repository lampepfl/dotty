package dotty.tools.dotc
package core

import Contexts.*, Symbols.*, Types.*, Flags.*
import Denotations.*, SymDenotations.*
import Names.Name, StdNames.nme
import ast.untpd

/** Extension methods for contexts where we want to keep the ctx.<methodName> syntax */
object ContextOps:

  extension (ctx: Context)

    /** Enter symbol into current class, if current class is owner of current context,
    *  or into current scope, if not. Should always be called instead of scope.enter
    *  in order to make sure that updates to class members are reflected in
    *  finger prints.
    */
    def enter(sym: Symbol): Symbol = inContext(ctx) {
      ctx.owner match
        case cls: ClassSymbol => cls.classDenot.enter(sym)
        case _ => ctx.scope.openForMutations.enter(sym)
      sym
    }

    /** The denotation with the given `name` and all `required` flags in current context
     */
    def denotNamed(name: Name, required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags): Denotation =
      inContext(ctx) {
        if (ctx.owner.isClass)
          if (ctx.outer.owner == ctx.owner) { // inner class scope; check whether we are referring to self
            if (ctx.scope.size == 1) {
              val elem = ctx.scope.lastEntry.nn
              if (elem.name == name) return elem.sym.denot // return self
            }
            val pre = ctx.owner.thisType
            if ctx.isJava then
              // Note: I didn't verify if there exists a code path that would require `lookInCompanion = true`,
              // it is just to preserve the original behavior.
              javaFindMember(name, pre, lookInCompanion = true, required, excluded)
            else pre.findMember(name, pre, required, excluded)
          }
          else // we are in the outermost context belonging to a class; self is invisible here. See inClassContext.
            if ctx.isJava then
              javaFindMember(name, ctx.owner.thisType, lookInCompanion = true,required, excluded)
            else
              ctx.owner.findMember(name, ctx.owner.thisType, required, excluded)
        else
          ctx.scope.denotsNamed(name).filterWithFlags(required, excluded).toDenot(NoPrefix)
      }

    /** Look in the prefix with Java semantics.
     * @param lookInCompanion  If true, try in the companion class of a module as a fallback.
     *                         Note: originally this was used to type Select nodes in Java code,
     *                         but that is no longer the case.
     *                         It is preserved in case it is necessary for denotNamed, but this is unverified.
     */
    final def javaFindMember(name: Name, pre: Type, lookInCompanion: Boolean, required: FlagSet = EmptyFlags, excluded: FlagSet = EmptyFlags): Denotation =
      assert(ctx.isJava)
      inContext(ctx) {
        import dotty.tools.dotc.core.NameOps.*
        val preSym = pre.typeSymbol
        // 1. Try to search in current type and parents.
        val directSearch =
          def asModule =
            if name.isTypeName && name.endsWith(StdNames.str.MODULE_SUFFIX) then
              pre.findMember(name.stripModuleClassSuffix.moduleClassName, pre, required, excluded)
            else NoDenotation
          pre.findMember(name, pre, required, excluded) match
            case NoDenotation => asModule
            case denot => denot

        // 2. Try to search in companion class if current is an object.
        def searchCompanionClass = if lookInCompanion && preSym.is(Flags.Module) then
          preSym.companionClass.thisType.findMember(name, pre, required, excluded)
          else NoDenotation

        // 3. Try to search in companion objects of super classes.
        // In Java code, static inner classes, which we model as members of the companion object,
        // can be referenced from an ident in a subclass or by a selection prefixed by the subclass.
        def searchSuperCompanionObjects =
          val toSearch = if preSym.is(Flags.Module) then
            if preSym.companionClass.exists then
              preSym.companionClass.asClass.baseClasses
            else Nil
          else
            preSym.asClass.baseClasses

          toSearch.iterator.map { bc =>
            val pre1 = bc.companionModule.namedType
            pre1.findMember(name, pre1, required, excluded)
          }.find(_.exists).getOrElse(NoDenotation)

        if preSym.isClass then
          directSearch orElse searchCompanionClass orElse searchSuperCompanionObjects
        else
          directSearch
      }

    /** A fresh local context with given tree and owner.
     *
     *  #19019 Self valdefs must always keep their enclosing ctx.owner. They
     *  can be NoSymbol or having a symbol that `isSelfSym`, depending on
     *  whether they have an explicit name or not. In either case, we avoid
     *  `setOwner`.
     *
     *  The owner might also not exist for other kinds of trees, such as
     *  `LambdaTypeTree` and `TermLambdaTypeTree`. In these cases, we also
     *  keep the enclosing owner.
     */
    def localContext(tree: untpd.Tree, owner: Symbol): FreshContext = inContext(ctx) {
      val freshCtx = ctx.fresh.setTree(tree)
      if owner.exists && !owner.isSelfSym then freshCtx.setOwner(owner) else freshCtx
    }

    /** Context where `sym` is defined, assuming we are in a nested context. */
    def defContext(sym: Symbol): Context = inContext(ctx) {
      ctx.outersIterator
        .dropWhile(_.owner != sym)
        .dropWhile(_.owner == sym)
        .next()
    }

    /** A new context for the interior of a class */
    def inClassContext(selfInfo: TypeOrSymbol): Context = inContext(ctx) {
      val localCtx: Context = ctx.fresh.setNewScope
      selfInfo match {
        case sym: Symbol if sym.exists && sym.name != nme.WILDCARD => localCtx.scope.openForMutations.enter(sym)
        case _ =>
      }
      localCtx
    }

    def packageContext(tree: untpd.PackageDef, pkg: Symbol): Context = inContext(ctx) {
      if (pkg.is(Package)) ctx.fresh.setOwner(pkg.moduleClass).setTree(tree)
      else ctx
    }
end ContextOps
