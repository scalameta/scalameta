package scala.meta
package internal
package ui

import org.scalameta.unreachable
import org.scalameta.show._

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.Universe

import scala.meta.internal.parsers.Helpers._
import scala.compat.Platform.EOL
import scala.meta.internal.tokenizers.keywords

import scala.meta.tokenquasiquotes._

import scala.language.implicitConversions

import scala.annotation.tailrec

// TODO: figure out various situation where postfix operators, etc. should be in parenthesis. For now: 
// it is always assumed.
// TODO: check creation of tokens
// TODO: figure out indentation (see below)
// TODO: checkout how to avoid the import problem of dialect for quasiquotes. 
//   => Send mail to Eugene tomorrow
private[meta] object inferTokens {

  def apply(tree: Tree): Tokens = {
    infer(tree)(scala.meta.dialects.Scala211)
  }

  /* TODO: remove in the future, this is here now for partial implementation
   * testing. */
  def generic(tree: Tree)(implicit dialect: Dialect): Tokens = {
    val code = tree.show[Code]
    (tree match {
      case _: Source => code.parse[Source]
      case _: Stat => code.parse[Stat]
    }).tokens
  }

  /* Generate tokens from various inputs */
  private def mineLitTk(value: Any)(implicit dialect: Dialect): Tokens = {
    implicit def stringToInput(str: String) = Input.String(str)
    val str = value.toString
    val length = str.length
    // TODO: figure out what action should be taken depending of the boolean
    // TODO: the strings here should be modified (e.g. adding L for long, etc.)
    val newTok = value match {
      case y: Int => Token.Literal.Int(str, dialect, 0, length, (x: Boolean) => y)
      case y: Long => Token.Literal.Long(str + "L", dialect, 0, length + 1, (x: Boolean) => y)
      case y: Float => Token.Literal.Float(str + "F", dialect, 0, length + 1, (x: Boolean) => y)
      case y: Double => Token.Literal.Double(str, dialect, 0, length, (x: Boolean) => y)
      case y: Char =>
        val newChar = enquote(str, SingleQuotes)
        Token.Literal.Char(newChar, dialect, 0, newChar.length, y)
      case y: Symbol => Token.Literal.Symbol(str, dialect, 0, length, y)
      case y: String =>
        val newStr = {
          if (y.contains(EOL)) enquote(str, TripleQuotes)
          else enquote(str, DoubleQuotes)
        }
        Token.Literal.String(newStr, dialect, 0, newStr.length, newStr)
    }
    Tokens(newTok)
  }

  /* Generate tokens for idents */
  private def mineIdentTk(value: String)(implicit dialect: Dialect): Tokens = Tokens(Token.Ident(Input.String(value), dialect, 0, value.length))

  /* Generate synthetic tokens */
  private def infer(tree: Tree)(implicit dialect: Dialect): Tokens = {
    import scala.meta.internal.ast._
    import scala.meta.dialects.Scala211 // TODO: figure out why the implicit in params is not enough

    val indentation = toks"  " // TODO: figure out how to find proper indent string
    val singleDoubleQuotes = Tokens(Token.Ident(Input.String("\""), dialect, 0, 1)) // TODO: for this line and below, figure out how to construct those without using Ident, which is a trick.
    val tripleDoubleQuotes = Tokens(Token.Ident(Input.String("\"\"\""), dialect, 0, 3))
    val dot = Tokens(Token.Ident(Input.String("."), dialect, 0, 1))
    val newline = Tokens(Token.`\n`(Input.String("\n"), dialect, 0))

    implicit class RichTree(tree: Tree) {
      def tks = tree.tokens
      def indTks = indent(tree.tokens)(indentation)
      def `->o->` = toks"$newline${tree.indTks}$newline"
    }

    implicit class RichTreeSeq(trees: Seq[Tree]) {

      /* Flatten tokens corresponding to a sequence of trees together. Can transform tokens corresponding to each tree using a first-order function. */
      def flattks(start: Tokens = toks"")(sep: Tokens = toks"", op: (Seq[Token] => Seq[Token]) = (x: Seq[Token]) => x)(end: Tokens = toks"") = {
        val sq = trees match {
          case Nil => toks""
          case _ => start ++ trees.init.flatMap(v => op(v.tks.repr) ++ sep) ++ op(trees.last.tks.repr) ++ end
        }
        Tokens(sq: _*)
      }

      /* various combiners for tokens:
       * - o   representing subsequence
       * - [   represent an indentation
       * - ->  represent a new line
       * - _   represent a space
       * -     other tokens represent themselves. 
       */
      def `oo` = flattks()()()
      def `o->o` = flattks()(newline)()
      def `->o->` = flattks(newline)(newline)(newline)
      def `->o` = flattks(newline)(newline)()
      def `o_o` = flattks()(toks" ")()
      def `o,o` = flattks()(toks", ")()
      def `o;o` = flattks()(toks"; ")()
      def `o_o_` = flattks()(toks" ")(toks" ")
      def `[o,o]` = flattks(toks"[")(toks", ")(toks"]")
      def `{o,o}` = flattks(toks"{")(toks", ")(toks"}")
      def `(o,o)` = flattks(toks"(")(toks", ")(toks")")
      def `[->o->` = flattks(newline)(newline, (s: Seq[Token]) => indent(s)(indentation))(newline)
      def `[->o` = flattks(newline)(newline, (s: Seq[Token]) => indent(s)(indentation))()
    }
    implicit class RichTreeSeqSeq(trees: Seq[Seq[Tree]]) {
    	// TODO: deduplicate
      def `(o,o)` = {
        val sq = trees match {
          case Nil => toks""
          case _ if trees.length == 1 && trees.head.length == 0 => toks"()"
          case _ => trees.flatMap(_.`(o,o)`)
        }
        Tokens(sq: _*)
      }
      def `[(o,o)` = {
        val sq = trees match {
          case Nil => toks""
          case _ if trees.length == 1 && trees.head.length == 0 => toks"()"
          case _ => trees.flatMap(_.`(o,o)`)
        }
        Tokens(sq: _*)
      }
    }
    implicit class RichTokensSeq(tkss: Seq[Tokens]) {
      def `oo` = Tokens(tkss.flatMap(_.repr): _*)
    }

    /* Append a template to a defn (class, trait, object) */
    def apndTempl(t: Template): Tokens = {
      val ext = if (!t.parents.isEmpty) toks" extends" else toks""
      val tpl = t.tks match {
        case tks if tks.repr.isEmpty => toks""
        case tks => toks" $tks"
      }
      toks"$ext$tpl"
    }
    /* append a declared type to a val / def / var */
    def apndDeclTpe(t: Option[Type]): Tokens = t match {
      case Some(tpe) if tpe.tks.length > 0 => toks": ${tpe.tks}"
      case _ => toks""
    }
    def apndTpeBounds(t: Type.Bounds): Tokens = {
      if (t.lo.isDefined || t.hi.isDefined) toks" ${t.tks}"
      else toks""
    }

    /* The helpers below are heavily based on the ones used for show[Code] */

    def guessIsBackquoted(t: Name): Boolean = {
      def cantBeWrittenWithoutBackquotes(t: Name): Boolean = {
        t.value != "this" && (keywords.contains(t.value) || t.value.contains(" "))
      }
      def isAmbiguousWithPatVarTerm(t: Term.Name, p: Tree): Boolean = {
        val looksLikePatVar = t.value.head.isLower && t.value.head.isLetter
        val thisLocationAlsoAcceptsPatVars = p match {
          case p: Term.Name => unreachable
          case p: Term.Select => false
          case p: Pat.Wildcard => unreachable
          case p: Pat.Var.Term => false
          case p: Pat.Bind => unreachable
          case p: Pat.Alternative => true
          case p: Pat.Tuple => true
          case p: Pat.Extract => p.args.exists(_ eq t)
          case p: Pat.ExtractInfix => (p.lhs eq t) || p.rhs.exists(_ eq t)
          case p: Pat.Interpolate => p.args.exists(_ eq t)
          case p: Pat.Typed => unreachable
          case p: Pat => unreachable
          case p: Case => p.pat eq t
          case p: Defn.Val => p.pats.exists(_ eq t)
          case p: Defn.Var => p.pats.exists(_ eq t)
          case p: Enumerator.Generator => p.pat eq t
          case p: Enumerator.Val => p.pat eq t
          case _ => false
        }
        looksLikePatVar && thisLocationAlsoAcceptsPatVars
      }
      def isAmbiguousWithPatVarType(t: Type.Name, p: Tree): Boolean = {
        // TODO: figure this out with Martin
        // `x match { case _: t => }` produces a Type.Name
        // `x match { case _: List[t] => }` produces a Pat.Var.Type
        // `x match { case _: List[`t`] => }` produces a Pat.Var.Type as well
        // the rules look really inconsistent and probably that's just an oversight
        false
      }
      (t, t.parent) match {
        case (t: Term.Name, Some(p: Tree)) => isAmbiguousWithPatVarTerm(t, p) || cantBeWrittenWithoutBackquotes(t)
        case (t: Type.Name, Some(p: Tree)) => isAmbiguousWithPatVarType(t, p) || cantBeWrittenWithoutBackquotes(t)
        case _ => cantBeWrittenWithoutBackquotes(t)
      }
    }
    def guessHasRefinement(t: Type.Compound): Boolean = t.refinement.nonEmpty
    def guessPatHasRefinement(t: Pat.Type.Compound): Boolean = t.refinement.nonEmpty
    def guessHasExpr(t: Term.Return): Boolean = t.expr match { case Lit.Unit() => false; case _ => true }
    def guessHasElsep(t: Term.If): Boolean = t.elsep match { case Lit.Unit() => false; case _ => true }
    def guessHasBraces(t: Pkg): Boolean = {
    def isOnlyChildOfOnlyChild(t: Pkg): Boolean = t.parent match {
      case Some(pkg: Pkg) => isOnlyChildOfOnlyChild(pkg) && pkg.stats.length == 1
      case Some(source: Source) => source.stats.length == 1
      case None => true
      case _ => unreachable
    }
    !isOnlyChildOfOnlyChild(t)
  }

    /* Infer tokens for a given tree, making use of the helpers above */
    def tkz(tree: Tree): Tokens = tree match {
      // Bottom
      case t: Quasi if t.rank > 0 =>
        val rank = Tokens((0 to t.rank) map (dot): _*)
        if (!t.tree.isInstanceOf[Quasi]) toks"$rank{${t.tree.asInstanceOf[Tree].tks}}"
        else rank
      case t: Quasi if t.rank == 0 =>
        val innerTreeTks = t.tree.asInstanceOf[Tree].tks
        val name = mineIdentTk(t.pt.getName.stripPrefix("scala.meta.").stripPrefix("internal.ast."))
        toks"$${$innerTreeTks @ $name}"

      // Name
      case t: Name.Anonymous => toks"_"
      case t: Name.Indeterminate =>
        if (guessIsBackquoted(t)) mineIdentTk("`" + t.value + "`")
        else mineIdentTk(t.value)

      // Term
      case t: Term if t.isCtorCall =>
        if (t.isInstanceOf[Ctor.Ref.Function]) toks"=>${t.ctorArgss.`(o,o)`}"
        else toks"${t.ctorTpe.tks}${t.ctorArgss.`(o,o)`}"
      case t: Term.This =>
        val qual = if (t.qual.isInstanceOf[Name.Anonymous]) toks"" else toks"${t.qual.tks}."
        toks"${qual}this"
      case t: Term.Super =>
        val thisqual = if (t.thisp.isInstanceOf[Name.Anonymous]) toks"" else toks"${t.thisp.tks}."
        val superqual = if (t.superp.isInstanceOf[Name.Anonymous]) toks"" else toks"[${t.superp.tks}]"
        toks"${thisqual}super${superqual}"
      case t: Term.Name =>
        if (guessIsBackquoted(t)) mineIdentTk("`" + t.value + "`")
        else mineIdentTk(t.value)
      case t: Term.Select => toks"${t.qual.tks}.${t.name.tks}"
      case t: Term.Interpolate =>
        val zipped = (t.parts zip t.args) map {
          case (part, id: Name) if !guessIsBackquoted(id) => toks"${mineIdentTk(part.value)}$$${mineIdentTk(id.value)}"
          case (part, args) => toks"${mineIdentTk(part.value)}$${${args.tks}}"
        }
        val quote: Tokens = if (t.parts.map(_.value).exists(s => s.contains(EOL) || s.contains("\""))) tripleDoubleQuotes else singleDoubleQuotes
        toks"${mineIdentTk(t.prefix.value)}$quote${zipped.`oo`}${mineIdentTk(t.parts.last.value)}$quote"
      case t: Term.Apply => toks"${t.fun.tks}${t.args.`(o,o)`}"
      case t: Term.ApplyType => toks"${t.fun.tks}${t.targs.`[o,o]`}"
      case t: Term.ApplyInfix =>
        val rhs = t.args match {
          case (arg: Term) :: Nil => toks"(${arg.tks})"
          case args => args.`(o,o)`
        }
        toks"(${t.lhs.tks} ${t.op.tks} $rhs)"
      case t: Term.ApplyUnary => toks"${t.op.tks}${t.arg.tks}"
      case t: Term.Assign => toks"${t.lhs.tks} = ${t.rhs.tks}"
      case t: Term.Update => toks"${t.fun.tks}${t.argss.`(o,o)`} = ${t.rhs.tks}"
      case t: Term.Return =>
        if (guessHasExpr(t)) toks"return ${t.expr.tks}"
        else toks"return"
      case t: Term.Throw => toks"throw ${t.expr.tks}"
      case t: Term.Ascribe => toks"(${t.expr.tks}: ${t.tpe.tks})"
      case t: Term.Annotate => toks"(${t.expr.tks}: ${t.annots.`o_o`})"
      case t: Term.Tuple => t.elements.`(o,o)`
      case t: Term.Block =>
        import Term.{ Block, Function }
        t match {
          case Block(Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, Block(stats)) :: Nil) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
            val tpt = tptopt.map(v => toks": ${v.tks}").getOrElse(toks"")
            toks"{ implicit ${name.tks}$tpt =>${stats.`[->o->`}}"
          case Block(Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, Block(stats)) :: Nil) =>
            toks"{ ${name.tks} =>${stats.`[->o->`}}"
          case Block(Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, Block(stats)) :: Nil) =>
            toks"{ _ =>${stats.`[->o->`}}"
          case Block(Function(Nil, Block(stats)) :: Nil) =>
            toks"{ () =>${stats.`[->o->`}}"
          case Block(Function(params, Block(stats)) :: Nil) if params.length == 1 =>
            toks"{ ${params.`o,o`} =>${stats.`[->o->`}}"
          case Block(Function(params, Block(stats)) :: Nil) =>
            toks"{ ${params.`(o,o)`} =>${stats.`[->o->`}}"
          case _ =>
            if (t.stats.isEmpty) toks"{}"
            else toks"{${t.stats.`[->o->`}}"
        }
      case t: Term.If =>
        if (guessHasElsep(t)) toks"if (${t.cond.tks}) ${t.thenp.tks} else ${t.elsep.tks}"
        else toks"if (${t.cond.tks}) ${t.thenp.tks}"
      case t: Term.Match => toks"(${t.scrut.tks} match {${t.cases.`[->o->`}})"
      case t: Term.TryWithCases =>
        val tryBlock = toks"try ${t.expr.tks}"
        val catchBlock = if (t.catchp.nonEmpty) toks" catch {${t.catchp.`[->o->`}}" else toks""
        val finallyBlock = if (t.finallyp.isDefined) toks" finally ${t.finallyp.get.tks}" else toks""
        tryBlock ++ catchBlock ++ finallyBlock
      case t: Term.TryWithTerm =>
        val tryBlock = toks"try ${t.expr.tks} catch {${t.catchp.`->o->`}}"
        val finallyBlock = if (t.finallyp.isDefined) toks" finally ${t.finallyp.get.tks}" else toks""
        tryBlock ++ finallyBlock
      case t: Term.Function =>
        t match {
          case Term.Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
            val tpt = tptopt.map(v => toks": ${v.tks}").getOrElse(toks"")
            toks"implicit ${name.tks}$tpt =>${body.tks}"
          case Term.Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, body) =>
            toks"${name.tks} => ${body.tks}"
          case Term.Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, body) =>
            toks"_ => ${body.tks}"
          case Term.Function(Nil, body) =>
            toks"() => ${body.tks}"
          case Term.Function(params, body) =>
            toks"${params.`(o,o)`} => ${body.tks}"
        }
      case t: Term.PartialFunction => toks"{${t.cases.`[->o->`}}"
      case t: Term.While => toks"while (${t.expr.tks}) ${t.body.tks}"
      case t: Term.Do => toks"do ${t.body.tks} while (${t.expr.tks})"
      case t: Term.For => toks"for (${t.enums.`o;o`}) ${t.body.tks}"
      case t: Term.ForYield => toks"for (${t.enums.`o;o`}) yield ${t.body.tks}"
      case t: Term.New => toks"new ${t.templ.tks}"
      case _: Term.Placeholder => toks"_"
      case t: Term.Eta => toks"${t.term.tks} _"
      case t: Term.Arg.Named => toks"${t.name.tks} = ${t.rhs.tks}"
      case t: Term.Arg.Repeated => toks"${t.arg.tks}: _*"
      case t: Term.Param =>
        val mods = t.mods.filter(!_.isInstanceOf[Mod.Implicit]) // NOTE: `implicit` in parameters is skipped in favor of `implicit` in the enclosing parameter list
        val tpe = t.decltpe.map(v => toks": ${v.tks}").getOrElse(toks"")
        val default = t.default.map(v => toks" = ${v.tks}").getOrElse(toks"")
        mods.`o_o_` ++ t.name.tks ++ tpe ++ default

      // Type
      case t: Type.Name =>
        if (guessIsBackquoted(t)) mineIdentTk("`" + t.value + "`")
        else mineIdentTk(t.value)
      case t: Type.Select => toks"${t.qual.tks}.${t.name.tks}"
      case t: Type.Project => toks"${t.qual.tks}#${t.name.tks}"
      case t: Type.Singleton => toks"${t.ref.tks}.type"
      case t: Type.Apply => toks"${t.tpe.tks}[${t.args.`o,o`}]"
      case t: Type.ApplyInfix => toks"(${t.lhs.tks} ${t.op.tks} ${t.rhs.tks})"
      case t: Type.Function =>
        val params = {
          if (t.params.isEmpty) toks"()"
          else if (t.params.size == 1) t.params.head.tks
          else t.params.`(o,o)`
        }
        toks"($params => ${t.res.tks})"
      case t: Type.Tuple => t.elements.`(o,o)`
      case t: Type.Compound =>
        val tpes = t.tpes.flattks()(toks" with ")()
        if (guessHasRefinement(t)) toks"$tpes { ${t.refinement.`o;o`} }"
        else tpes
      case t: Type.Existential => toks"${t.tpe.tks} forSome { ${t.quants.`o;o`} }"
      case t: Type.Annotate => toks"${t.tpe.tks} ${t.annots.`o_o`}"
      case t: Type.Placeholder => toks"_ ${t.bounds.tks}"
      case t: Type.Bounds =>
        val loOpt = t.lo.map(lo => toks">: ${lo.tks}")
        val hiOpt = t.hi.map(hi => toks"<: ${hi.tks}")
        (loOpt, hiOpt) match {
          case (None, None) => toks""
          case (Some(hi), None) => hi
          case (None, Some(lo)) => lo
          case (Some(hi), Some(lo)) => toks"$hi $lo"
        }
      case t: Type.Arg.Repeated => toks"${t.tpe.tks}*"
      case t: Type.Arg.ByName => toks"=> ${t.tpe.tks}"
      case t: Type.Param =>
        val mods = t.mods.filter(m => !m.isInstanceOf[Mod.Covariant] && !m.isInstanceOf[Mod.Contravariant])
        require(t.mods.length - mods.length <= 1)
        val variance = t.mods.foldLeft(toks"")((curr, m) => if (m.isInstanceOf[Mod.Covariant]) toks"+" else if (m.isInstanceOf[Mod.Contravariant]) toks"-" else curr)
        val tbounds = t.typeBounds.tks
        val vbounds = t.viewBounds.flattks(toks" <% ")(toks" <% ")()
        val cbounds = t.contextBounds.flattks(toks": ")(toks": ")()
        toks"${mods.o_o}$variance${t.name.tks}${t.tparams.`oo`}$tbounds$vbounds$cbounds"

      // Pat
      case t: Pat.Var.Term => mineIdentTk(t.name.value)
      case _: Pat.Wildcard => toks"_"
      case t: Pat.Bind =>
        val separator: Tokens = if (t.rhs.isInstanceOf[Pat.Arg.SeqWildcard] && dialect.bindToSeqWildcardDesignator == ":") toks"" else toks" "
        val designator: Tokens = if (t.rhs.isInstanceOf[Pat.Arg.SeqWildcard] && dialect.bindToSeqWildcardDesignator == ":") toks":" else toks"@"
        toks"${t.lhs.tks}$separator$designator ${t.rhs.tks}"
      case t: Pat.Alternative => toks"${t.lhs.tks} | ${t.rhs.tks}"
      case t: Pat.Tuple => t.elements.`(o,o)`
      case t: Pat.Extract => toks"${t.ref.tks}${t.targs.`[o,o]`}${t.args.`(o,o)`}"
      case t: Pat.ExtractInfix =>
        t.rhs match {
          case x :: Nil => toks"(${t.lhs.tks} ${t.ref.tks} ${x.tks})"
          case _ => toks"(${t.lhs.tks} ${t.ref.tks} ${t.rhs.`(o,o)`})"
        }
      case t: Pat.Interpolate =>
        val zipped = (t.parts zip t.args) map {
          case (part, Pat.Var.Term(id: Name)) if !guessIsBackquoted(id) => toks"${mineIdentTk(part.value)}$$${mineIdentTk(id.value)}"
          case (part, args) => toks"${mineIdentTk(part.value)}$${${args.tks}}"
        }
        toks"${mineIdentTk(t.prefix.value)}$singleDoubleQuotes${zipped.`oo`}${mineIdentTk(t.parts.last.value)}$singleDoubleQuotes"
      case t: Pat.Typed => toks"(${t.lhs.tks}: ${t.rhs.tks})"
      case _: Pat.Arg.SeqWildcard => toks"_*"

      // Pat.Type
      // TODO: fix copy/paste with Type
      case t: Pat.Type.Wildcard => toks"_"
      case t: Pat.Var.Type => mineIdentTk(t.name.value)
      case t: Pat.Type.Project => toks"${t.qual.tks}#${t.name.tks}"
      case t: Pat.Type.Apply => toks"${t.tpe.tks}${t.args.`[o,o]`}"
      case t: Pat.Type.ApplyInfix => toks"(${t.lhs.tks} ${t.op.tks} ${t.rhs.tks})"
      case t: Pat.Type.Function =>
        val params = if (t.params.size == 1) t.params.head.tks else t.params.`(o,o)`
        toks"$params => ${t.res.tks}"
      case t: Pat.Type.Tuple => t.elements.`(o,o)`
      case t: Pat.Type.Compound =>
        val tpes = t.tpes.flattks()(toks" with ")()
        if (guessPatHasRefinement(t)) toks"$tpes { ${t.refinement.`o;o`} }"
        else tpes
      case t: Pat.Type.Existential => toks"${t.tpe.tks} forSome { ${t.quants.`o;o`} }"
      case t: Pat.Type.Annotate => toks"${t.tpe.tks} ${t.annots.`o_o`}"

      // Lit
      case t: Lit.Bool if t.value => toks"true"
      case t: Lit.Bool if !t.value => toks"false"
      case t: Lit.Byte => mineLitTk(t.value)
      case t: Lit.Short => mineLitTk(t.value)
      case t: Lit.Int => mineLitTk(t.value)
      case t: Lit.Long => mineLitTk(t.value)
      case t: Lit.Float => mineLitTk(t.value)
      case t: Lit.Double => mineLitTk(t.value)
      case t: Lit.Char => mineLitTk(t.value)
      case t: Lit.String => mineLitTk(t.value)
      case t: Lit.Symbol => mineLitTk(t.value)
      case _: Lit.Null => toks"null"
      case _: Lit.Unit => toks"()"

      // Member
      case t: Decl.Val => toks"${t.mods.`o_o_`}val ${t.pats.`oo`}: ${t.decltpe.tks}"
      case t: Decl.Var => toks"${t.mods.`o_o_`}var ${t.pats.`oo`}: ${t.decltpe.tks}"
      case t: Decl.Type => toks"${t.mods.`o_o_`}type ${t.name.tks}${t.tparams.`[o,o]`}${apndTpeBounds(t.bounds)}"
      case t: Decl.Def => toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}: ${t.decltpe.tks}"
      case t: Defn.Val => toks"${t.mods.`o_o_`}val ${t.pats.`oo`}${apndDeclTpe(t.decltpe)} = ${t.rhs.tks}"
      case t: Defn.Var =>
        val rhs = t.rhs.map(trm => toks" = ${trm.tks}").getOrElse(toks"")
        toks"${t.mods.`o_o_`}var ${t.pats.`oo`}${apndDeclTpe(t.decltpe)}$rhs"
      case t: Defn.Type => toks"${t.mods.`o_o_`}type ${t.name.tks}${t.tparams.`[o,o]`} = ${t.body.tks}"
      case t: Defn.Class => toks"${t.mods.`o_o_`}class ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks}${apndTempl(t.templ)}"
      case t: Defn.Trait => toks"${t.mods.`o_o_`}trait ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks}${apndTempl(t.templ)}"
      case t: Defn.Object => toks"${t.mods.`o_o_`}object ${t.name.tks}${t.ctor.tks}${apndTempl(t.templ)}"
      case t: Defn.Def => toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}${apndDeclTpe(t.decltpe)} = ${t.body.tks}"
      case t: Defn.Macro => toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}: ${t.tpe.tks} = macro ${t.body.tks}"
      case t: Pkg => 
      	if(guessHasBraces(t)) toks"package ${t.ref.tks}{${t.stats.`[->o`}}"
      	else toks"package ${t.ref.tks}${t.stats.`->o`}"
      case t: Pkg.Object => toks"package ${t.mods.`o_o_`}object ${t.name.tks}${apndTempl(t.templ)}"
      case t: Ctor.Primary =>
        if (t.mods.nonEmpty && t.paramss.nonEmpty) toks"${t.mods.`o_o_`}${t.paramss.`(o,o)`}"
        else toks"${t.paramss.`(o,o)`}"
      case t: Ctor.Secondary =>
        if (t.body.isInstanceOf[Term.Block]) toks"def this${t.paramss.`(o,o)`} ${t.body.tks}"
        else toks"def this${t.paramss.`(o,o)`} = ${t.body.tks}"

      // Template
      case t: Template =>
        val isSelfEmpty = t.self.name.isInstanceOf[Name.Anonymous] && t.self.decltpe.isEmpty
        val isSelfNonEmpty = !isSelfEmpty
        val isBodyEmpty = isSelfEmpty && t.stats.isEmpty
        val isTemplateEmpty = t.early.isEmpty && t.parents.isEmpty && isBodyEmpty
        if (isTemplateEmpty) toks""
        else {
          val pearly = if (!t.early.isEmpty) toks"{ ${t.early.`o;o`} } with " else toks""
          val pparents = {
            if (!t.parents.isEmpty) t.parents.flattks()(toks" with ")(toks" ")
            else toks""
          }
          val pbody = {
            val isOneLiner = t.stats.map(stats => stats.length == 0 || (stats.length == 1 && !stats.head.tokens.map(_.show[Code]).mkString.contains(EOL))).getOrElse(true)
            (isSelfNonEmpty, t.stats.nonEmpty, t.stats.getOrElse(Nil)) match {
              case (false, false, _) => toks""
              case (true, false, _) => toks"{ ${t.self.tks} => }"
              case (false, true, Seq()) if isOneLiner => toks"{}"
              case (false, true, Seq(stat)) if isOneLiner => toks"{ ${stat.tks} }"
              case (false, true, stats) => toks"{${stats.`[->o->`}}"
              case (true, true, Seq()) if isOneLiner => toks"{ ${t.self.tks} => }"
              case (true, true, Seq(stat)) if isOneLiner => toks"{ ${t.self.tks} => ${stat.tks} }"
              case (true, true, stats) => toks"{ ${t.self.tks} =>${stats.`[->o->`}}"
            }
          }
          toks"$pearly$pparents$pbody"
        }

      // Mod
      case Mod.Annot(tree) => toks"@${tree.ctorTpe.tks}${tree.ctorArgss.`[(o,o)`}"
      case Mod.Private(Name.Anonymous()) => toks"private"
      case Mod.Private(name) => toks"private[${name.tks}]"
      case Mod.Protected(Name.Anonymous()) => toks"protected"
      case Mod.Protected(name) => toks"protected[${name.tks}]"
      case _: Mod.Implicit => toks"implicit"
      case _: Mod.Final => toks"final"
      case _: Mod.Sealed => toks"sealed"
      case _: Mod.Override => toks"override"
      case _: Mod.Case => toks"case"
      case _: Mod.Abstract => toks"abstract"
      case _: Mod.Covariant => toks"+"
      case _: Mod.Contravariant => toks"-"
      case _: Mod.Lazy => toks"lazy"
      case _: Mod.ValParam => toks"val"
      case _: Mod.VarParam => toks"var"
      case Mod.Ffi(signature) =>
        val quote = if (signature.contains(EOL)) tripleDoubleQuotes else singleDoubleQuotes
        toks"@ffi($quote${mineLitTk(signature)}$quote)"

      // Enumerator
      case t: Enumerator.Val => toks"${t.pat.tks} = ${t.rhs.tks}"
      case t: Enumerator.Generator => toks"${t.pat.tks} <- ${t.rhs.tks}"
      case t: Enumerator.Guard => toks"if ${t.cond.tks}"

      // Import
      case t: Import.Selector.Name => toks"${t.value.tks}"
      case t: Import.Selector.Rename => toks"${t.from.tks} => ${t.to.tks}"
      case t: Import.Selector.Unimport => toks"${t.name.tks} => _"
      case _: Import.Selector.Wildcard => toks"_"
      case t: Import.Clause =>
        if (t.sels.size == 1 && !t.sels.head.isInstanceOf[Import.Selector.Rename]
          && !t.sels.head.isInstanceOf[Import.Selector.Unimport]) toks"${t.ref.tks}.${t.sels.`oo`}"
        else toks"${t.ref.tks}.${t.sels.`{o,o}`}"
      case t: Import => toks"import ${t.clauses.`o,o`}"

      // Case
      case t: Case =>
        val ppat = t.pat.tks
        val pcond = t.cond.map(cond => toks" if ${cond.tks}").getOrElse(toks"")
        val isOneLiner = {
          def isOneLiner(t: Case) = t.stats.length == 0 || (t.stats.length == 1 && !t.stats.head.tokens.map(_.show[Code]).mkString.contains(EOL))
          t.parent match {
            case Some(Term.Match(_, cases)) => cases.forall(isOneLiner)
            case Some(Term.PartialFunction(cases)) => cases.forall(isOneLiner)
            case _ => isOneLiner(t)
          }
        }
        val pbody = (t.stats, isOneLiner) match {
          case (Nil, true) => toks""
          case (List(stat), true) => toks" ${stat.tks}"
          case (stats, _) => stats.`[->o`
        }
        toks"case ${t.pat.tks}$pcond =>$pbody"

      // Source
      case t: Source => t.stats.`o->o`
    }

    tkz(tree.asInstanceOf[scala.meta.internal.ast.Tree])
  }

  // TODO: check what is the best way to do that. The problem is as follow 
  //
  // 1. Indentation is already present in original token streams.
  //   - We can't just add one indentation token
  //   - Moreover, this is even more true that indentation is not added to a synthetic tree
  //   i.e. let say a tree t2 (synth.), which itself contain a tree t3 (original). Then if
  //        t2 was inside a tree t, we can't simply add indentation to all the lines in the 
  //        token stream of t2, as t3 already had some kind of indentation present.
  //
  // 2. Indentation is not present in generated code
  //   - But it is practically impossible to infer this at a call to inferToken,
  //   even if a subtree has some knowledge of its parent (but not the tokens corresponding 
  //   to its parents - it would lead to recursion explosion).
  //
  // A solution could be:
  // 1. Tokens contain inputs. we can skip indentation for all tokens with real inputs.
  // 2. We can add one indentation shift to all tokens with virtual inputs.
  // 3. This will have to be based on: 1) the input type; 2) the position in a line.
  // Line return could be checked at runtime.

  /* Adding proper indentation to the token stream */

  // TODO: clean that up, it is far from perfect.
  private def indent(tks: Seq[Token], withoutBounds: Boolean = false)(indent: Tokens)(implicit dialect: Dialect): Tokens = {
    import scala.meta.dialects.Scala211 // TODO: remove, figure that out!	
    val onLine = {
      @tailrec def loop(in: Seq[Token], out: Seq[Seq[Token]]): Seq[Seq[Token]] = in match {
        case Nil => out
        case _ if !in.exists(_.code == "\n") => out :+ in
        case _ =>
          val (bf, af) = in.span(_.code != "\n")
          loop(af.tail, out :+ (bf :+ af.head))
      }
      loop(tks.repr, Seq())
    }
    // step2: check, for each line, if the line contain synthetic tokens
    // step2.1: if so, indent it once
    // step2.2: if not, do nothing
    val toIndent = if (withoutBounds && onLine.length > 1) onLine.tail.init else onLine
    val indented = toIndent map (line =>toks"$indent$line".repr)
    if (withoutBounds && onLine.length > 1) Tokens((onLine.head ++ indented.flatten ++ onLine.last): _*)
    else Tokens(indented.flatten: _*)
  }

  // TODO: check if required
  // Calls to the token quasiquote is not doing it so far.

  /* Put back all the positions at the righ places in a sequence of tokens. */
  private def inplace(tks: Tokens): Tokens = {
    tks // TODO
  }

}
