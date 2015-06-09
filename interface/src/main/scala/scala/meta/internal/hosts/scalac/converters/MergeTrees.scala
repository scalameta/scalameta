package scala.meta
package internal.hosts.scalac
package converters

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._

import scala.collection.{ immutable => imm }

/*
 * TODO: This is a temporary solution allowing to have semantic information added to parsed trees, which contain layout
 * information such as Tokens.

 * Sometimes, converted trees contain more information than their parsed equivalents in their structure: for instance, Updates
 * and Tuples are already converted as such. Sometimes it happens that namespaces are added, transforming Names into
 * Selects. Some of those information are interesting at the semantic level, so we transfer them to the parsed tree.
 * We do that only when required, as we want the form of the parsed tree to prevail in case of non classified differences.
 *
 * I am more and more concern regarding the fact that this might not be a viable solution for the long term. As mentioned,
 * it might be better to parse the tree and add semantic information from scalac directly on top of it rather than
 * converting it. The current merging of trees relies on many layers:
 * First, the conversion by toMTree must not leave any ambiguity, and then some ambiguities might be added by the fact
 * that the two tree differ during the merging. It does the trick for tools like Obey while running on complex projects
 * (Tested on various Play apps), but it might return warnings in some corner cases were not covered. This would have to
 * be tested more thoroughly.
 */
object MergeTrees {

  def apply(parsedTree: m.Source, convertedTree: m.Source): m.Source = {

    def zLoop[T](f: (T, T) => T)(pTree: imm.Seq[T], cTree: imm.Seq[T]): imm.Seq[T] = (pTree zip cTree).map(s => f(s._1, s._2).asInstanceOf[T])

    def zzLoop[T](f: (T, T) => T)(pTree: imm.Seq[imm.Seq[T]], cTree: imm.Seq[imm.Seq[T]]): imm.Seq[imm.Seq[T]] = {
      (pTree zip cTree).map(xs => (xs._1 zip xs._2).map(x => f(x._1, x._2).asInstanceOf[T]))
    }

    def oLoop[T](f: (T, T) => T)(pTree: Option[T], cTree: Option[T]): Option[T] = (pTree, cTree) match {
      case (Some(p), Some(c)) => Some(f(p, c).asInstanceOf[T])
      case _ => pTree
    }

    // TODO: also copy typings!
    def loop[T <: m.Tree](pTree: T, cTree: T): T = {
      import m._
      ((pTree, cTree) match {

        /* Handling normal cases */

        // Names
        case (p: Term.Name, c: Term.Name) =>
          p.copy(denot = c.denot, tokens = p.tokens)
        case (p: Type.Name, c: Type.Name) =>
          p.copy(denot = c.denot, tokens = p.tokens)
        case (p: Ctor.Ref.Name, c: Ctor.Ref.Name) =>
          p.copy(denot = c.denot, tokens = p.tokens)
        case (p: Name.Anonymous, c: Name.Anonymous) =>
          p.copy(denot = c.denot, tokens = p.tokens)
        case (p: Name.Indeterminate, c: Name.Indeterminate) =>
          p.copy(denot = c.denot, tokens = p.tokens)

        // Terms
        case (p: Term.This, c: Term.This) =>
          p.copy(loop(p.qual, c.qual), tokens = p.tokens)
        case (p: Term.Super, c: Term.Super) =>
          p.copy(loop(p.thisp, c.thisp), loop(p.superp, c.superp), tokens = p.tokens)
        case (p: Term.Select, c: Term.Select) =>
          p.copy(loop(p.qual, c.qual), loop(p.name, c.name), tokens = p.tokens)
        case (p: Term.Interpolate, c: Term.Interpolate) =>
          p.copy(loop(p.prefix, c.prefix), zLoop(loop[Lit.String])(p.parts, c.parts), zLoop(loop[Term])(p.args, c.args), tokens = p.tokens)
        case (p: Term.Apply, c: Term.Apply) =>
          p.copy(loop(p.fun, c.fun), zLoop(loop[Term.Arg])(p.args, c.args), tokens = p.tokens)
        case (p: Term.ApplyType, c: Term.ApplyType) =>
          p.copy(loop(p.fun, c.fun), zLoop(loop[Type])(p.targs, c.targs), tokens = p.tokens)
        case (p: Term.ApplyInfix, c: Term.ApplyInfix) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.op, c.op), zLoop(loop[Type])(p.targs, c.targs), zLoop(loop[Term.Arg])(p.args, c.args), tokens = p.tokens)
        case (p: Term.ApplyUnary, c: Term.ApplyUnary) =>
          p.copy(loop(p.op, c.op), loop(p.arg, c.arg), tokens = p.tokens)
        case (p: Term.Assign, c: Term.Assign) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Term.Update, c: Term.Update) =>
          p.copy(loop(p.fun, c.fun), zzLoop(loop[Term.Arg])(p.argss, c.argss), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Term.Return, c: Term.Return) =>
          p.copy(loop(p.expr, c.expr), tokens = p.tokens)
        case (p: Term.Throw, c: Term.Throw) =>
          p.copy(loop(p.expr, c.expr), tokens = p.tokens)
        case (p: Term.Ascribe, c: Term.Ascribe) =>
          p.copy(loop(p.expr, c.expr), loop(p.decltpe, c.decltpe), tokens = p.tokens)
        case (p: Term.Annotate, c: Term.Annotate) =>
          p.copy(loop(p.expr, c.expr), zLoop(loop[Mod.Annot])(p.annots, c.annots), tokens = p.tokens)
        case (p: Term.Tuple, c: Term.Tuple) =>
          p.copy(zLoop(loop[Term])(p.elements, c.elements), tokens = p.tokens)
        case (p: Term.Block, c: Term.Block) =>
          p.copy(zLoop(loop[Stat])(p.stats, c.stats), tokens = p.tokens)
        case (p: Term.If, c: Term.If) =>
          p.copy(loop(p.cond, c.cond), loop(p.thenp, c.thenp), loop(p.elsep, c.elsep), tokens = p.tokens)
        case (p: Term.Match, c: Term.Match) =>
          p.copy(loop(p.scrut, c.scrut), zLoop(loop[Case])(p.cases, c.cases), tokens = p.tokens)
        case (p: Term.TryWithCases, c: Term.TryWithCases) =>
          p.copy(loop(p.expr, c.expr), zLoop(loop[Case])(p.catchp, c.catchp), oLoop(loop[Term])(p.finallyp, c.finallyp), tokens = p.tokens)
        case (p: Term.TryWithTerm, c: Term.TryWithTerm) =>
          p.copy(loop(p.expr, c.expr), loop(p.catchp, c.catchp), oLoop(loop[Term])(p.finallyp, c.finallyp), tokens = p.tokens)
        case (p: Term.Function, c: Term.Function) =>
          p.copy(zLoop(loop[Term.Param])(p.params, c.params), loop(p.body, c.body), tokens = p.tokens)
        case (p: Term.PartialFunction, c: Term.PartialFunction) =>
          p.copy(zLoop(loop[Case])(p.cases, c.cases), tokens = p.tokens)
        case (p: Term.While, c: Term.While) =>
          p.copy(loop(p.expr, c.expr), loop(p.body, c.body), tokens = p.tokens)
        case (p: Term.Do, c: Term.Do) =>
          p.copy(loop(p.body, c.body), loop(p.expr, c.expr), tokens = p.tokens)
        case (p: Term.For, c: Term.For) =>
          p.copy(zLoop(loop[Enumerator])(p.enums, c.enums), loop(p.body, c.body), tokens = p.tokens)
        case (p: Term.ForYield, c: Term.ForYield) =>
          p.copy(zLoop(loop[Enumerator])(p.enums, c.enums), loop(p.body, c.body), tokens = p.tokens)
        case (p: Term.New, c: Term.New) =>
          p.copy(loop(p.templ, c.templ), tokens = p.tokens)
        case (p: Term.Placeholder, c: Term.Placeholder) =>
          p
        case (p: Term.Eta, c: Term.Eta) =>
          p.copy(loop(p.term, c.term), tokens = p.tokens)
        case (p: Term.Arg.Named, c: Term.Arg.Named) =>
          p.copy(loop(p.name, c.name), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Term.Arg.Repeated, c: Term.Arg.Repeated) =>
          p.copy(loop(p.arg, c.arg), tokens = p.tokens)
        case (p: Term.Param, c: Term.Param) =>
          p.copy(
            zLoop(loop[Mod])(p.mods, c.mods),
            loop(p.name, c.name),
            oLoop(loop[Type.Arg])(p.decltpe, c.decltpe),
            oLoop(loop[Term])(p.default, c.default), tokens = p.tokens)

        // Types
        case (p: Type.Select, c: Type.Select) =>
          p.copy(loop(p.qual, c.qual), loop(p.name, c.name), tokens = p.tokens)
        case (p: m.Type.Project, c: Type.Project) =>
          p.copy(loop(p.qual, c.qual), loop(p.name, c.name), tokens = p.tokens)
        case (p: Type.Singleton, c: Type.Singleton) =>
          p.copy(loop(p.ref, c.ref), tokens = p.tokens)
        case (p: Type.Apply, c: m.Type.Apply) =>
          p.copy(loop(p.tpe, c.tpe), zLoop(loop[Type])(p.args, c.args), tokens = p.tokens)
        case (p: Type.ApplyInfix, c: Type.ApplyInfix) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.op, c.op), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Type.Function, c: Type.Function) =>
          p.copy(zLoop(loop[Type.Arg])(p.params, c.params), loop(p.res, c.res), tokens = p.tokens)
        case (p: Type.Tuple, c: Type.Tuple) =>
          p.copy(zLoop(loop[Type])(p.elements, c.elements), tokens = p.tokens)
        case (p: Type.Compound, c: Type.Compound) =>
          p.copy(zLoop(loop[Type])(p.tpes, c.tpes), zLoop(loop[Stat])(p.refinement, c.refinement), tokens = p.tokens)
        case (p: Type.Existential, c: Type.Existential) =>
          p.copy(loop(p.tpe, c.tpe), zLoop(loop[Stat])(p.quants, c.quants), tokens = p.tokens)
        case (p: Type.Annotate, c: Type.Annotate) =>
          p.copy(loop(p.tpe, c.tpe), zLoop(loop[Mod.Annot])(p.annots, c.annots), tokens = p.tokens)
        case (p: Type.Placeholder, c: Type.Placeholder) =>
          p
        case (p: Type.Arg.ByName, c: Type.Arg.ByName) =>
          p.copy(loop(p.tpe, c.tpe), tokens = p.tokens)
        case (p: Type.Arg.Repeated, c: Type.Arg.Repeated) =>
          p.copy(loop(p.tpe, c.tpe), tokens = p.tokens)
        case (p: Type.Param, c: Type.Param) =>
          p.copy(
            zLoop(loop[Mod])(p.mods, c.mods),
            loop(p.name, c.name),
            zLoop(loop[Type.Param])(p.tparams, c.tparams),
            loop(p.typeBounds, c.typeBounds),
            zLoop(loop[Type])(p.viewBounds, c.viewBounds),
            zLoop(loop[Type])(p.contextBounds, c.contextBounds), tokens = p.tokens)
        case (p: Type.Bounds, c: Type.Bounds) =>
          p.copy(oLoop(loop[Type])(p.lo, c.lo), oLoop(loop[Type])(p.hi, c.hi), tokens = p.tokens)

        // Stats
        case (p: Decl.Val, c: Decl.Val) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), zLoop(loop[Pat.Var.Term])(p.pats, c.pats), loop(p.decltpe, c.decltpe), tokens = p.tokens)
        case (p: Decl.Var, c: Decl.Var) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), zLoop(loop[Pat.Var.Term])(p.pats, c.pats), loop(p.decltpe, c.decltpe), tokens = p.tokens)
        case (p: Decl.Def, c: Decl.Def) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), zzLoop(loop[Term.Param])(p.paramss, c.paramss), loop(p.decltpe, c.decltpe), tokens = p.tokens)
        case (p: Decl.Type, c: Decl.Type) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), loop(p.bounds, c.bounds), tokens = p.tokens)
        case (p: Defn.Val, c: Defn.Val) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), zLoop(loop[Pat])(p.pats, c.pats), oLoop(loop[Type])(p.decltpe, c.decltpe), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Defn.Var, c: Defn.Var) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), zLoop(loop[Pat])(p.pats, c.pats), oLoop(loop[Type])(p.decltpe, c.decltpe), oLoop(loop[Term])(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Defn.Def, c: Defn.Def) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), zzLoop(loop[Term.Param])(p.paramss, c.paramss), oLoop(loop[Type])(p.decltpe, c.decltpe), loop(p.body, c.body), tokens = p.tokens)
        case (p: Defn.Macro, c: Defn.Macro) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), zzLoop(loop[Term.Param])(p.paramss, c.paramss), loop(p.decltpe, c.decltpe), loop(p.body, c.body), tokens = p.tokens)
        case (p: Defn.Type, c: Defn.Type) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), loop(p.body, c.body), tokens = p.tokens)
        case (p: Defn.Class, c: Defn.Class) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), loop(p.ctor, c.ctor), loop(p.templ, c.templ), tokens = p.tokens)
        case (p: Defn.Trait, c: Defn.Trait) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zLoop(loop[Type.Param])(p.tparams, c.tparams), loop(p.ctor, c.ctor), loop(p.templ, c.templ), tokens = p.tokens)
        case (p: Defn.Object, c: Defn.Object) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), loop(p.ctor, c.ctor), loop(p.templ, c.templ), tokens = p.tokens)
        case (p: Pkg, c: Pkg) =>
          p.copy(loop(p.ref, c.ref), zLoop(loop[Stat])(p.stats, c.stats), tokens = p.tokens)

        // Imports
        case (p: Import, c: Import) =>
          p.copy(zLoop(loop[Import.Clause])(p.clauses, c.clauses), tokens = p.tokens)
        case (p: Import.Clause, c: Import.Clause) =>
          p.copy(loop(p.ref, c.ref), zLoop(loop[Import.Selector])(p.sels, c.sels), tokens = p.tokens)
        case (p: Import.Selector.Wildcard, c: Import.Selector.Wildcard) =>
          p
        case (p: Import.Selector.Name, c: Import.Selector.Name) =>
          p.copy(loop(p.value, c.value), tokens = p.tokens)
        case (p: Import.Selector.Rename, c: Import.Selector.Rename) =>
          p.copy(loop(p.from, c.from), loop(p.to, c.to), tokens = p.tokens)
        case (p: Import.Selector.Unimport, c: Import.Selector.Unimport) =>
          p.copy(loop(p.name, c.name), tokens = p.tokens)

        // Case
        case (p: Case, c: Case) =>
          p.copy(loop(p.pat, c.pat), oLoop(loop[Term])(p.cond, c.cond), loop(p.body, c.body), tokens = p.tokens)

        // Lits
        case (p: Lit, c: Lit) => p

        // Mods
        case (p: Mod.Annot, c: Mod.Annot) =>
          p.copy(loop(p.body, c.body), tokens = p.tokens)
        case (p: Mod.Private, c: Mod.Private) =>
          p.copy(loop(p.within, c.within), tokens = p.tokens)
        case (p: Mod.Protected, c: Mod.Protected) =>
          p.copy(loop(p.within, c.within), tokens = p.tokens)
        case (p: Mod, c: Mod) =>
          p

        // Pat Terms
        case (p: Pat.Var.Term, c: Pat.Var.Term) =>
          p.copy(loop(p.name, c.name), tokens = p.tokens)
        case (p: Pat.Wildcard, c: Pat.Wildcard) =>
          p
        case (p: Pat.Bind, c: Pat.Bind) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Pat.Alternative, c: Pat.Alternative) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Pat.Tuple, c: Pat.Tuple) =>
          p.copy(zLoop(loop[Pat])(p.elements, c.elements), tokens = p.tokens)
        case (p: Pat.Extract, c: Pat.Extract) =>
          p.copy(loop(p.ref, c.ref), zLoop(loop[Type])(p.targs, c.targs), zLoop(loop[Pat.Arg])(p.args, c.args), tokens = p.tokens)
        case (p: Pat.ExtractInfix, c: Pat.ExtractInfix) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.ref, c.ref), zLoop(loop[Pat.Arg])(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Pat.Interpolate, c: Pat.Interpolate) =>
          p.copy(
            loop(p.prefix, c.prefix),
            zLoop(loop[Lit.String])(p.parts, c.parts),
            zLoop(loop[Pat])(p.args, c.args), tokens = p.tokens)
        case (p: Pat.Typed, c: Pat.Typed) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Pat.Arg, c: Pat.Arg) =>
          p

        // Pat Types
        case (p: Pat.Var.Type, c: Pat.Var.Type) =>
          p.copy(loop(p.name, c.name), tokens = p.tokens)
        case (p: Pat.Type.Project, c: Pat.Type.Project) =>
          p.copy(loop(p.qual, c.qual), loop(p.name, c.name), tokens = p.tokens)
        case (p: Pat.Type.Apply, c: Pat.Type.Apply) =>
          p.copy(loop(p.tpe, c.tpe), zLoop(loop[Pat.Type])(p.args, c.args), tokens = p.tokens)
        case (p: Pat.Type.ApplyInfix, c: Pat.Type.ApplyInfix) =>
          p.copy(loop(p.lhs, c.lhs), loop(p.op, c.op), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Pat.Type.Function, c: Pat.Type.Function) =>
          p.copy(zLoop(loop[Pat.Type])(p.params, c.params), loop(p.res, c.res), tokens = p.tokens)
        case (p: Pat.Type.Tuple, c: Pat.Type.Tuple) =>
          p.copy(zLoop(loop[Pat.Type])(p.elements, c.elements), tokens = p.tokens)
        case (p: Pat.Type.Compound, c: Pat.Type.Compound) =>
          p.copy(zLoop(loop[Pat.Type])(p.tpes, c.tpes), zLoop(loop[Stat])(p.refinement, c.refinement), tokens = p.tokens)
        case (p: Pat.Type.Existential, c: Pat.Type.Existential) =>
          p.copy(loop(p.tpe, c.tpe), zLoop(loop[Stat])(p.quants, c.quants), tokens = p.tokens)
        case (p: Pat.Type.Annotate, c: Pat.Type.Annotate) =>
          p.copy(loop(p.tpe, c.tpe), zLoop(loop[Mod.Annot])(p.annots, c.annots), tokens = p.tokens)

        // Enumerator
        case (p: Enumerator.Generator, c: Enumerator.Generator) =>
          p.copy(loop(p.pat, c.pat), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Enumerator.Val, c: Enumerator.Val) =>
          p.copy(loop(p.pat, c.pat), loop(p.rhs, c.rhs), tokens = p.tokens)
        case (p: Enumerator.Guard, c: Enumerator.Guard) =>
          p.copy(loop(p.cond, c.cond), tokens = p.tokens)

        // Template
        case (p: Template, c: Template) =>
          p.copy(
            zLoop(loop[Stat])(p.early, c.early),
            zLoop(loop[Term])(p.parents, c.parents),
            loop(p.self, c.self),
            oLoop(zLoop(loop[Stat]))(p.stats, c.stats), tokens = p.tokens)

        // Ctors
        case (p: Ctor.Primary, c: Ctor.Primary) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zzLoop(loop[Term.Param])(p.paramss, c.paramss), tokens = p.tokens)
        case (p: Ctor.Secondary, c: Ctor.Secondary) =>
          p.copy(zLoop(loop[Mod])(p.mods, c.mods), loop(p.name, c.name), zzLoop(loop[Term.Param])(p.paramss, c.paramss), loop(p.body, c.body), tokens = p.tokens)
        case (p: Ctor.Ref.Select, c: Ctor.Ref.Select) =>
          p.copy(loop(p.qual, c.qual), loop(p.name, c.name), tokens = p.tokens)
        case (p: Ctor.Ref.Project, c: Ctor.Ref.Project) =>
          p.copy(loop(p.qual, c.qual), loop(p.name, c.name), tokens = p.tokens)
        case (p: Ctor.Ref.Function, c: Ctor.Ref.Function) =>
          p.copy(loop(p.name, c.name), tokens = p.tokens)

        // Sources
        case (p: Source, c: Source) =>
          p.copy(zLoop(loop[Stat])(p.stats, c.stats), tokens = p.tokens)

        /* Handling special cases */

        // Since the converted trees contain more information and have been reorganized (Tuples insteads of specific Apply, Update insteads of specific Apply)
        // We reproduce here the shape of the converted tree.

        // Ex.
        // p: Ctor.Ref.name("deprecated"),
        // c: Ctor.Ref.Select(Term.Name("scala"), Ctor.Ref.Name("deprecated"))
        case (p: Ctor.Ref.Name, Ctor.Ref.Select(n, c1: Ctor.Ref.Name)) if p.value == c1.value =>
          loop(p, c1)

        // Block containing only one statement
        case (p: Term.Block, c: Stat) if p.stats.length == 1 =>
          loop(p.stats.head, c)
        case (p: Stat, c: Term.Block) if c.stats.length == 1 =>
          loop(p, c.stats.head)

        // Applying a term to no argument at all
        case (p: Term.Apply, c: Term) if p.args.length == 0 =>
          loop(p.fun, c)

        // Ex.
        // p: Term.Apply(Term.Select(Term.Name("Properties"), Term.Name("update")), List(Lit.String("currentValidity"), Term.Select(Term.Name("secs"), Term.Name("toString"))))
        // c: Term.Update(Term.Name("Properties"), List(List(Lit.String("currentValidity"))), Term.Select(Term.Name("secs"), Term.Name("toString")))
        case (p @ Term.Apply(p1: Term.Select, pargs), Term.Update(c1, cargs, c2)) if cargs.size == 1 && cargs.head.size == 1 =>
          Term.Update(loop(p1.qual, c1), List(zLoop(loop[Term.Arg])(pargs.init, cargs.head)), loop(pargs.last.asInstanceOf[Term], c2), tokens = p.tokens)

        // Ex.
        // p: Type.Apply(Type.Select(Term.Name("scala"), Type.Name("Tuple2")), List(Type.Name("Date"), Type.Name("Date")))
        // c: Type.Tuple(List(Type.Name("Date"), Type.Name("Date")))
        case (p: Type.Apply, Type.Tuple(celems)) =>
          Type.Tuple(zLoop(loop[Type])(p.args, celems), tokens = p.tokens)

        // Convering name expantion equivalent as for Ctor
        case (p: m.Type.Name, c: m.Type.Select) if  p.value == c.name.value =>
          loop(p, c.name)

        /* Generic case: parsed tree prevails */

        // Macros are sometimes expanded, etc. In such a case, this part of tree might lack semantic information.
        case (p, c) =>
          p
      }).asInstanceOf[T]
    }
    loop(parsedTree, convertedTree)
  }
}
