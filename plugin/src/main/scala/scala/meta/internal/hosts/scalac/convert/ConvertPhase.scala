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
          case _ => pTree // TODO: error
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

        def loopQual(pTree: api.Name.Qualifier, cTree: api.Name.Qualifier): api.Name.Qualifier = pTree // TODO

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
              p.copy(zLoop(p.params, c.params, loopParam), loopTerm(p.body, c.body))
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
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopTermArg(pTree: api.Term.Arg, cTree: api.Term.Arg): api.Term.Arg = {
          import api.Term.Arg._
          ((pTree, cTree) match {
            case (p: Named, c: Named) =>
             p.copy(loopName(p.name, c.name).asInstanceOf[api.Term.Name], loopTerm(p.rhs, c.rhs))
            case (p: Repeated, c: Repeated) =>
              p.copy(loopTerm(p.arg, c.arg))
          }).appendScratchpad(cTree.scratchpad)
        }

        def loopParam(p: api.Term.Param, c: api.Term.Param): api.Term.Param = {
          import api.Term.Param._
          p.copy(zLoop(p.mods, c.mods, loopMod), loopName(p.name, c.name).asInstanceOf[Name], oLoop(p.decltpe, c.decltpe, loopTypeArg), oLoop(p.default, c.default, loopTerm)).appendScratchpad(c.scratchpad)
        }

        def loopType(pTree: api.Type, cTree: api.Type): api.Type = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopTypeArg(pTree: api.Type.Arg, cTree: api.Type.Arg): api.Type.Arg = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopSource(pTree: api.Source, cTree: api.Source): api.Source = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopLit(pTree: api.Lit, cTree: api.Lit): api.Lit = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopMod(pTree: api.Mod, cTree: api.Mod): api.Mod = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopStat(pTree: api.Stat, cTree: api.Stat): api.Stat = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopCase(pTree: api.Case, cTree: api.Case): api.Case = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopEnum(pTree: api.Enumerator, cTree: api.Enumerator): api.Enumerator = pTree.appendScratchpad(cTree.scratchpad) // TODO
        def loopTempl(pTree: api.Template, cTree: api.Template): api.Template = pTree.appendScratchpad(cTree.scratchpad) // TODO

        loopSource(parsedTree, convertedTree)
      }

      override def apply(unit: CompilationUnit) {
        println(unit.source.path)
        val parsedTree = unit.source.content.mkString("").parse[Source].asInstanceOf[api.Source]
        val convertedTree = c.toMtree(unit.body, classOf[Source]).asInstanceOf[api.Source]

        unit.body.appendMetadata("scalameta" -> merge(parsedTree, convertedTree))
      }

    }
  }
}