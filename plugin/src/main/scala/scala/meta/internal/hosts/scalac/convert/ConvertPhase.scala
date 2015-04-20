package scala.meta
package internal.hosts.scalac
package convert

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._

import scala.meta.internal.{ ast => api}
import scala.meta.tql._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

import scala.collection.{immutable => imm}

trait ConvertPhase {
  self: ScalahostPlugin =>

  object ConvertComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._

    // TODO: ideally we would like to save everything after the very end of typechecking, which is after refchecks
    // but unfortunately by then a lot of semantic stuff is already desugared to death (patmat, superaccessors, some code in refchecks)
    // therefore we run after typer and hope for the best (i.e. that we don't run into nonsense that we don't know how to convert,
    // and also that we don't encounter residual cyclic reference errors which are the reason why certain typechecks are delayed past typer)
    // btw this isn't such a big problem for persistence, but it definitely is for macro interpretation
    // let's hope that the research into runtime macros, which entails moving the typechecker to scala-reflect.jar will allow us to restructure things
    // so that delayed typechecks come right after typer, not intermingled with other logic
    override val runsAfter = List("typer")
    override val runsRightAfter = None
    val phaseName = "convert"
    override def description = "convert compiler trees to scala.meta"
    implicit val c = Scalahost.mkGlobalContext[global.type](global)

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {

      private def merge(parsedTree: api.Source, convertedTree: api.Source): api.Source = {

        def zLoop[T](pTree: imm.Seq[T], cTree: imm.Seq[T], f: (T,T) => api.Tree): imm.Seq[T] = (pTree zip cTree).map(s => f(s._1, s._2).asInstanceOf[T])

        def zzLoop[T](pTree: imm.Seq[imm.Seq[T]], cTree: imm.Seq[imm.Seq[T]], f: (T, T) => api.Tree): imm.Seq[imm.Seq[T]] = {
          (pTree zip cTree).map(xs => (xs._1 zip xs._2).map(x => f(x._1, x._2).asInstanceOf[T]))
        }

        def oLoop[T](pTree: Option[T], cTree: Option[T], f: (T, T) => api.Tree): Option[T] = (pTree, cTree) match {
          case (Some(p), Some(c)) => Some(f(p,c).asInstanceOf[T])
          case (None, None) => None
          case _ =>
            reporter.warning(NoPosition, "An error occured while merging the parsed and the converted tree. The trees where not identical. This should never happen.")
            pTree
        }

        def loopName(pTree: api.Name, cTree: api.Name): api.Name = {
          ((pTree, cTree) match {
            case (p: api.Term.Name, c: api.Term.Name) =>
              p.copy(denot = c.denot, sigma = c.sigma)
            case (p: api.Type.Name, c: api.Type.Name) =>
              p.copy(denot = c.denot, sigma = c.sigma)
            case (p: api.Ctor.Ref.Name, c: api.Ctor.Ref.Name) =>
              p.copy(denot = c.denot, sigma = c.sigma)
            case (p: api.Name.Anonymous, c: api.Name.Anonymous) =>
              p.copy(denot = c.denot, sigma = c.sigma)
            case (p: api.Name.Indeterminate, c: api.Name.Indeterminate) =>
              p.copy(denot = c.denot, sigma = c.sigma)
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopQual(pTree: api.Name.Qualifier, cTree: api.Name.Qualifier): api.Name.Qualifier = pTree.appendScratchpad(cTree.scratchpad)

        def loopTerm(pTree: api.Term, cTree: api.Term): api.Term = {
          import api.Term._
          ((pTree, cTree) match {
            case (p: This, c: This) =>
              p.copy(loopQual(p.qual, c.qual))
            case (p: Super, c: Super) =>
              p.copy(loopQual(p.thisp, c.thisp), loopQual(p.superp, c.superp))
            case (p: Select, c: Select) =>
              p.copy(loopTerm(p.qual, c.qual), loopName(p.name, c.name).asInstanceOf[Name])
            case (p: Interpolate, c: Interpolate) =>
              p.copy(loopName(p.prefix, c.prefix).asInstanceOf[Name], zLoop(p.parts, c.parts, loopLit), zLoop(p.args, c.args, loopTerm))
            case (p: Apply, c: Apply) =>
              p.copy(loopTerm(p.fun, c.fun), zLoop(p.args, c.args, loopTermArg))
            case (p: ApplyType, c: ApplyType) =>
              p.copy(loopTerm(p.fun, c.fun), zLoop(p.targs, c.targs, loopType))
            case (p: ApplyInfix, c: ApplyInfix) =>
              p.copy(loopTerm(p.lhs, c.lhs), loopName(p.op, c.op).asInstanceOf[Name], zLoop(p.targs, c.targs, loopType), zLoop(p.args, c.args, loopTermArg))
            case (p: ApplyUnary, c : ApplyUnary) =>
              p.copy(loopName(p.op, c.op).asInstanceOf[Name], loopTerm(p.arg, c.arg))
            case (p: Assign, c: Assign) =>
              p.copy(loopTerm(p.lhs, c.lhs).asInstanceOf[Ref], loopTerm(p.rhs, c.rhs))
            case (p: Update, c: Update) =>
              p.copy(loopTerm(p.fun, c.fun), zzLoop(p.argss, c.argss, loopTermArg), loopTerm(p.rhs, c.rhs))
            case (p: Return, c: Return) =>
              p.copy(loopTerm(p.expr, c.expr))
            case (p: Throw, c: Throw) =>
              p.copy(loopTerm(p.expr, c.expr))
            case (p: Ascribe, c: Ascribe) =>
              p.copy(loopTerm(p.expr, c.expr), loopType(p.tpe, c.tpe))
            case (p: Annotate, c: Annotate) =>
              p.copy(loopTerm(p.expr, c.expr), zLoop(p.annots, c.annots, loopMod))
            case (p: Tuple, c: Tuple) =>
              p.copy(zLoop(p.elements, c.elements, loopTerm))
            case (p: Block, c: Block) =>
              p.copy(zLoop(p.stats, c.stats, loopStat))
            case (p: If, c: If) =>
              p.copy(loopTerm(p.cond, c.cond), loopTerm(p.thenp, c.thenp), loopTerm(p.elsep, c.elsep))
            case (p: Match, c: Match) =>
              p.copy(loopTerm(p.scrut, c.scrut), zLoop(p.cases, c.cases, loopCase))
            case (p: TryWithCases, c: TryWithCases) =>
              p.copy(loopTerm(p.expr, c.expr), zLoop(p.catchp, c.catchp, loopCase), oLoop(p.finallyp, c.finallyp, loopTerm))
            case (p: TryWithTerm, c: TryWithTerm) =>
              p.copy(loopTerm(p.expr, c.expr), loopTerm(p.catchp, c.catchp), oLoop(p.finallyp, c.finallyp, loopTerm))
            case (p: Function, c: Function) =>
              p.copy(zLoop(p.params, c.params, loopTermParam), loopTerm(p.body, c.body))
            case (p: PartialFunction, c: PartialFunction) =>
              p.copy(zLoop(p.cases, c.cases, loopCase))
            case (p: While, c: While) =>
              p.copy(loopTerm(p.expr, c.expr), loopTerm(p.body, c.body))
            case (p: Do, c: Do) =>
              p.copy(loopTerm(p.body, c.body), loopTerm(p.expr, c.expr))
            case (p: For, c: For) =>
              p.copy(zLoop(p.enums, c.enums, loopEnum), loopTerm(p.body, c.body))
            case (p: ForYield, c: ForYield) =>
              p.copy(zLoop(p.enums, c.enums, loopEnum), loopTerm(p.body, c.body))
            case (p: New, c: New) =>
              p.copy(loopTempl(p.templ, c.templ))
            case (p: Placeholder, c: Placeholder) =>
              p
            case (p: Eta, c: Eta) =>
              p.copy(loopTerm(p.term, c.term))
            case (p: api.Lit, c: api.Lit) => loopLit(p, c)
            case (p: Name, c: Name) => loopName(p,c).asInstanceOf[Name]
            case (p: api.Ctor.Name, c: api.Ctor.Name) => loopName(p,c).asInstanceOf[api.Ctor.Name]
            // Handling special cases
            case (p: Block, c: api.Stat) if p.stats.length == 1 =>
              p.copy(imm.Seq(loopStat(p.stats.head, c)))
            case (p: Interpolate, c: Apply) => p // TODO: this is due to quasiquote expansion - should probably be left as is in the parsed tree.
            case (p, c) => p
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopTermArg(pTree: api.Term.Arg, cTree: api.Term.Arg): api.Term.Arg = {
          import api.Term.Arg._
          ((pTree, cTree) match {
            case (p: Named, c: Named) =>
             p.copy(loopName(p.name, c.name).asInstanceOf[api.Term.Name], loopTerm(p.rhs, c.rhs))
            case (p: Repeated, c: Repeated) =>
              p.copy(loopTerm(p.arg, c.arg))
            case (p: api.Term, c: api.Term) => loopTerm(p, c)
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopTermParam(p: api.Term.Param, c: api.Term.Param): api.Term.Param = {
          import api.Term.Param._
          p.copy(
            zLoop(p.mods, c.mods, loopMod),
            loopName(p.name, c.name).asInstanceOf[Name],
            oLoop(p.decltpe, c.decltpe, loopTypeArg),
            oLoop(p.default, c.default, loopTerm)
          ).appendScratchpad(c.scratchpad)
        }

        def loopType(pTree: api.Type, cTree: api.Type): api.Type = {
          import api.Type._
          ((pTree, cTree) match {
            case (p: Select, c: Select) =>
              p.copy(loopTerm(p.qual, c.qual).asInstanceOf[api.Term.Ref], loopName(p.name, c.name).asInstanceOf[Name])
            case (p: Project, c: Project) =>
              p.copy(loopType(p.qual, c.qual), loopName(p.name, c.name).asInstanceOf[Name])
            case (p: Singleton, c: Singleton) =>
              p.copy(loopTerm(p.ref, c.ref).asInstanceOf[api.Term.Ref])
            case (p: Apply, c: Apply) =>
              p.copy(loopType(p.tpe, c.tpe), zLoop(p.args, c.args, loopType))
            case (p: ApplyInfix, c: ApplyInfix) =>
              p.copy(loopType(p.lhs, c.lhs), loopName(p.op, c.op).asInstanceOf[Name], loopType(p.rhs, c.rhs))
            case (p: Function, c: Function) =>
              p.copy(zLoop(p.params, c.params, loopTypeArg), loopType(p.res, c.res))
            case (p: Tuple, c: Tuple) =>
              p.copy(zLoop(p.elements, c.elements, loopType))
            case (p: Compound, c: Compound) =>
              p.copy(zLoop(p.tpes, c.tpes, loopType), zLoop(p.refinement, c.refinement, loopStat))
            case (p: Existential, c: Existential) =>
              p.copy(loopType(p.tpe, c.tpe), zLoop(p.quants, c.quants, loopStat))
            case (p: Annotate, c:Annotate) =>
              p.copy(loopType(p.tpe, c.tpe), zLoop(p.annots, c.annots, loopMod))
            case (p: Placeholder, c: Placeholder) => p
            case (p: Name, c: Name) => loopName(p,c).asInstanceOf[Name]
            case (p, c) => p
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopTypeArg(pTree: api.Type.Arg, cTree: api.Type.Arg): api.Type.Arg = {
          import api.Type.Arg._
          ((pTree, cTree) match {
            case (p: ByName, c: ByName) =>
              p.copy(loopType(p.tpe, c.tpe))
            case (p: Repeated, c: Repeated) =>
              p.copy(loopType(p.tpe, c.tpe))
            case (p: api.Type, c: api.Type) => loopType(p, c)
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopTypeParam(pTree: api.Type.Param, cTree: api.Type.Param): api.Type.Param = {
          import api.Type.Param._
          pTree.copy(
            zLoop(pTree.mods, cTree.mods, loopMod),
            loopName(pTree.name, cTree.name).asInstanceOf[Name],
            zLoop(pTree.tparams, cTree.tparams, loopTypeParam),
            loopBounds(pTree.typeBounds, cTree.typeBounds),
            zLoop(pTree.viewBounds, cTree.viewBounds, loopType),
            zLoop(pTree.contextBounds, cTree.contextBounds, loopType)
          ).appendScratchpad(cTree.scratchpad)
        }

        def loopBounds(pTree: api.Type.Bounds, cTree: api.Type.Bounds): api.Type.Bounds = {
          pTree.copy(oLoop(pTree.lo, cTree.lo, loopType), oLoop(pTree.hi, cTree.hi, loopType)).appendScratchpad(cTree.scratchpad)
        }

        def loopStat(pTree: api.Stat, cTree: api.Stat): api.Stat = {
          import api._
          ((pTree, cTree) match {
            case (p: Decl.Val, c: Decl.Val) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), zLoop(p.pats, c.pats, loopPat), loopType(p.decltpe, c.decltpe))
            case (p: Decl.Var, c: Decl.Var) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), zLoop(p.pats, c.pats, loopPat), loopType(p.decltpe, c.decltpe))
            case (p: Decl.Def, c: Decl.Def) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Term.Name], zLoop(p.tparams, c.tparams, loopTypeParam), zzLoop(p.paramss, c.paramss, loopTermParam), loopType(p.decltpe, c.decltpe))
            case (p: Decl.Type, c: Decl.Type) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Type.Name], zLoop(p.tparams, c.tparams, loopTypeParam), loopBounds(p.bounds, c.bounds))
            case (p: Defn.Val, c: Defn.Val) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), zLoop(p.pats, c.pats, loopPat), oLoop(p.decltpe, c.decltpe, loopType), loopTerm(p.rhs, c.rhs))
            case (p: Defn.Var, c: Defn.Var) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), zLoop(p.pats, c.pats, loopPat), oLoop(p.decltpe, c.decltpe, loopType), oLoop(p.rhs, c.rhs, loopTerm))
            case (p: Defn.Def, c: Defn.Def) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Term.Name], zLoop(p.tparams, c.tparams, loopTypeParam), zzLoop(p.paramss, c.paramss, loopTermParam), oLoop(p.decltpe, c.decltpe, loopType), loopTerm(p.body, c.body))
            case (p: Defn.Macro, c: Defn.Macro) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Term.Name], zLoop(p.tparams, c.tparams, loopTypeParam), zzLoop(p.paramss, c.paramss, loopTermParam), loopType(p.tpe, c.tpe), loopTerm(p.body, c.body))
            case (p: Defn.Type, c: Defn.Type) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Type.Name], zLoop(p.tparams, c.tparams, loopTypeParam), loopType(p.body, c.body))
            case (p: Defn.Class, c: Defn.Class) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Type.Name], zLoop(p.tparams, c.tparams, loopTypeParam), loopCtor(p.ctor, c.ctor).asInstanceOf[Ctor.Primary], loopTempl(p.templ, c.templ))
            case (p: Defn.Trait, c: Defn.Trait) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Type.Name], zLoop(p.tparams, c.tparams, loopTypeParam), loopCtor(p.ctor, c.ctor).asInstanceOf[Ctor.Primary], loopTempl(p.templ, c.templ))
            case (p: Defn.Object, c: Defn.Object) =>
              p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Term.Name], loopCtor(p.ctor, c.ctor).asInstanceOf[Ctor.Primary], loopTempl(p.templ, c.templ))
            case (p: Pkg, c: Pkg) =>
              p.copy(loopTerm(p.ref, c.ref).asInstanceOf[Term.Ref], zLoop(p.stats, c.stats, loopStat))
            case (p: Import, c: Import) =>
              def loopSelector(pTree: Import.Selector, cTree: Import.Selector): Import.Selector = {
                import Import.Selector._
                ((pTree, cTree) match {
                  case (p: Wildcard, c: Wildcard) => p
                  case (p: Name, c: Name) => p.copy(loopName(p.value, c.value).asInstanceOf[api.Name.Indeterminate])
                  case (p: Rename, c: Rename) => p.copy(loopName(p.from, c.from).asInstanceOf[api.Name.Indeterminate], loopName(p.to, c.to).asInstanceOf[api.Name.Indeterminate])
                  case (p: Unimport, c: Unimport) => p.copy(loopName(p.name, c.name).asInstanceOf[api.Name.Indeterminate])
                }).appendScratchpad(cTree.scratchpad)
              }
              def loopClause(p: Import.Clause, c: Import.Clause): Import.Clause = p.copy(loopTerm(p.ref, c.ref).asInstanceOf[Term.Ref], zLoop(p.sels, c.sels, loopSelector))
              p.copy(zLoop(p.clauses, c.clauses, loopClause))
            case (p: Term, c: Term) => loopTerm(p, c)
              // handling special case
            case (p: Defn.Def, c: Defn.Macro) => p // TODO: this seems to be due to a bug (Quasiquotes are expanded here)
            case (p: Defn.Macro, c: Defn.Def) => p // TODO: this seems to be due to a bug (Quasiquotes are expanded here)
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopCase(pTree: api.Case, cTree: api.Case): api.Case = pTree.copy(
          loopPat(pTree.pat, cTree.pat),
          oLoop(pTree.cond, cTree.cond, loopTerm),
          loopTerm(pTree.body, cTree.body).asInstanceOf[api.Term.Block]
        ).appendScratchpad(cTree.scratchpad)

        def loopLit(pTree: api.Lit, cTree: api.Lit): api.Lit = pTree.appendScratchpad(cTree.scratchpad)

        def loopMod(pTree: api.Mod, cTree: api.Mod): api.Mod = {
          import api.Mod._
          ((pTree, cTree) match {
            case (p: Annot, c: Annot) =>
              p.copy(loopTerm(p.body, c.body))
            case (p: Private, c: Private) =>
              p.copy(loopQual(p.within, c.within))
            case (p: Protected, c: Protected) =>
              p.copy(loopQual(p.within, c.within))
            case (p, _) => p
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopPat(pTree: api.Pat, cTree: api.Pat): api.Pat = {
          import api._
          ((pTree, cTree) match {
            case (p: Pat.Var.Term, c: Pat.Var.Term) =>
              p.copy(loopName(p.name, c.name).asInstanceOf[Term.Name])
            case (p: Pat.Var.Type, c: Pat.Var.Type) =>
              p.copy(loopName(p.name, c.name).asInstanceOf[Type.Name])
            case (p: Pat.Wildcard, c: Pat.Wildcard) =>
              p
            case (p: Pat.Bind, c: Pat.Bind) =>
              p.copy(loopPat(p.lhs, c.lhs).asInstanceOf[Pat.Var.Term], p.rhs.appendScratchpad(c.rhs.scratchpad))
            case (p: Pat.Alternative, c: Pat.Alternative) =>
              p.copy(loopPat(p.lhs, c.lhs), loopPat(p.rhs, c.rhs))
            case (p: Pat.Tuple, c: Pat.Tuple) =>
              p.copy(zLoop(p.elements, c.elements, loopPat))
            case (p: Pat.Extract, c: Pat.Extract) =>
              p.copy(loopTerm(p.ref, c.ref).asInstanceOf[Term.Ref], zLoop(p.targs, c.targs, loopType), zLoop(p.args, c.args, loopPatArg))
            case (p: Pat.ExtractInfix, c: Pat.ExtractInfix) =>
              p.copy(loopPat(p.lhs, c.lhs), loopName(p.ref, c.ref).asInstanceOf[Term.Name], zLoop(p.rhs, c.rhs, loopPatArg))
            case (p: Pat.Interpolate, c: Pat.Interpolate) =>
              p.copy(
                loopName(p.prefix, c.prefix).asInstanceOf[Term.Name],
                zLoop(p.parts, c.parts, loopLit).map(_.asInstanceOf[Lit.String]),
                zLoop(p.args, c.args, loopPat)
              )
            case (p: Pat.Typed, c: Pat.Typed) =>
              p.copy(loopPat(p.lhs, c.lhs), loopPatType(p.rhs, c.rhs))
            case (p: Name, c: Name) => loopName(p,c).asInstanceOf[Term.Name]
          })
          pTree.appendScratchpad(cTree.scratchpad)
        }

        def loopPatArg(pTree: api.Pat.Arg, cTree: api.Pat.Arg): api.Pat.Arg = pTree.appendScratchpad(cTree.scratchpad)

        def loopPatType(pTree: api.Pat.Type, cTree: api.Pat.Type): api.Pat.Type =  {
          import api.Pat.Type._
          ((pTree, cTree) match {
            case (p: Wildcard, c: Wildcard) =>
              p
            case (p: Project, c: Project) =>
              p.copy(loopPatType(p.qual, c.qual), loopName(p.name, c.name).asInstanceOf[api.Type.Name])
            case (p: Apply, c: Apply) =>
              p.copy(loopPatType(p.tpe, c.tpe), zLoop(p.args, c.args, loopPatType))
            case (p: ApplyInfix, c: ApplyInfix) =>
              p.copy(loopPatType(p.lhs, c.lhs), loopName(p.op, c.op).asInstanceOf[api.Type.Name], loopPatType(p.rhs, c.rhs))

            case (p: Function, c: Function) =>
              p.copy(zLoop(p.params, c.params, loopPatType), loopPatType(p.res, c.res))
            case (p: Tuple, c: Tuple) =>
              p.copy(zLoop(p.elements, c.elements, loopPatType))
            case (p: Compound, c: Compound) =>
              p.copy(zLoop(p.tpes, c.tpes, loopPatType), zLoop(p.refinement, c.refinement, loopStat))
            case (p: Existential, c: Existential) =>
              p.copy(loopPatType(p.tpe, c.tpe), zLoop(p.quants, c.quants, loopStat))
            case (p: Annotate, c:Annotate) =>
              p.copy(loopPatType(p.tpe, c.tpe), zLoop(p.annots, c.annots, loopMod))
            case (p: api.Type.Name, c: api.Type.Name) => loopName(p, c).asInstanceOf[api.Type.Name]
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopEnum(pTree: api.Enumerator, cTree: api.Enumerator): api.Enumerator = {
          import api._
          ((pTree, cTree) match {
            case (p: Enumerator.Generator, c: Enumerator.Generator) => p.copy(loopPat(p.pat, c.pat), loopTerm(p.rhs, c.rhs))
            case (p: Enumerator.Val, c: Enumerator.Val) => p.copy(loopPat(p.pat, c.pat), loopTerm(p.rhs, c.rhs))
            case (p: Enumerator.Guard, c: Enumerator.Guard) => p.copy(loopTerm(p.cond, c.cond))
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopTempl(pTree: api.Template, cTree: api.Template): api.Template = pTree.copy(
          zLoop(pTree.early, cTree.early, loopStat),
          zLoop(pTree.parents, cTree.parents, loopTerm),
          loopTermParam(pTree.self, cTree.self),
            (pTree.stats, cTree.stats) match {
              case (Some(p), Some(c)) => Some(zLoop(p, c, loopStat))
              case (_, _) => None
            }
          ).appendScratchpad(cTree.scratchpad)

        def loopCtor(pTree: api.Ctor, cTree: api.Ctor): api.Ctor = {
          import api.Ctor._
          ((pTree, cTree) match {
            case (p: Primary, c: Primary) => p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Name], zzLoop(p.paramss, c.paramss, loopTermParam))
            case (p: Secondary, c: Secondary) => p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Name], zzLoop(p.paramss, c.paramss, loopTermParam), loopTerm(p.body, c.body))
          }).appendScratchpad(cTree.scratchpad)
        }


        def loopSource(pTree: api.Source, cTree: api.Source): api.Source = {
          pTree.copy(zLoop(pTree.stats, cTree.stats, loopStat)).appendScratchpad(cTree.scratchpad)
        }

        loopSource(parsedTree, convertedTree)
      }

      override def apply(unit: CompilationUnit) {
        val parsedTree = unit.source.content.mkString("").parse[Source].asInstanceOf[api.Source]
        val convertedTree = c.toMtree(unit.body, classOf[Source]).asInstanceOf[api.Source]

        // TODO: remove
        //val mergedTree = merge(parsedTree, convertedTree)
        //println(mergedTree.show[Semantics])

        unit.body.appendMetadata("scalameta" -> merge(parsedTree, convertedTree))

      }

    }
  }
}