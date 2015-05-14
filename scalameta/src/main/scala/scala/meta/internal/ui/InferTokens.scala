package scala.meta
package internal
package ui

import org.scalameta.unreachable

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.Universe

import scala.meta.internal.tokenizers.keywords
import scala.meta.tokenquasiquotes._

import scala.language.implicitConversions

// TODO: check for BOF and EOF, EOL, etc.
// TODO: see for specific cases, as in showCode
private[meta] object inferTokens {

  def apply(tree: Tree)(implicit dialect: Dialect): Tokens = {
    infer(tree)
  }

  /* TODO: remove in the future, this is here now for partial implementation
   * testing. */
  def generic(tree: Tree)(implicit dialect: Dialect): Tokens = {
    val code = tree.show[Code]
        (tree match {
            case _: Source => code.parse[Source]
            case _: Stat => code.parse[Stat]
        }).origin.tokens
  }

  /* Generate tokens from various inputs */
  private def mineLitTk(value: Any): Tokens = {
    implicit def stringToInput(str: String) = Input.String(str)
    val str = value.toString
    val length = str.length
    // TODO: figure out what action should be taken depending of the boolean
    // TODO: the strings here should be modified (e.g. adding L for long, etc.)
    val newTok = value match {
        case y: Int =>      Token.Literal.Int(str, 0, length, (x: Boolean) => y)
        case y: Long =>     Token.Literal.Long(str + "L", 0, length + 1, (x: Boolean) => y)
        case y: Float =>    Token.Literal.Float(str + "F", 0, length + 1, (x: Boolean) => y)
        case y: Double =>   Token.Literal.Double(str, 0, length, (x: Boolean) => y)
        case y: Char =>     Token.Literal.Char("'" + str + "'", 0, length + 2, y)
        case y: Symbol =>   Token.Literal.Symbol(str, 0, length, y)
        case y: String =>   Token.Literal.String("\"" + str + "\"", 0, length + 2, y)
    }
    Tokens(newTok)
  }

  /* Generate synthetic tokens */
  private def infer(tree: Tree)(implicit dialect: Dialect): Tokens = {
    import scala.meta.internal.ast._
    import scala.meta.dialects.Scala211 // TODO: figure out why the implicit in params is not enough
  
    implicit def toTokenSeq(tk: Token) = Tokens(tk)
  
    val indentation = toks"  " // TODO: figure out how to find proper indent string
    implicit class RichTree(tree: Tree) {
      def tks = indent(tree.tokens)(indentation)
    }

    implicit class RichTreeSeq(trees: Seq[Tree]) {
  
      /* Flatten tokens corresponding to a sequence of trees together */
      def flattks(start: Tokens = toks"")(sep: Tokens = toks"")(end: Tokens = toks"") = {
          val sq = trees match {
              case Nil => toks""
              case _ => start ++ trees.init.flatMap(_.tokens.repr ++ sep) ++ trees.last.tokens.repr ++ end
          }
          Tokens(sq: _*)
      }
  
      /* various combiners for tokens, o representing subsequence */
      def `oo` = flattks()()()
      // TODO: this is disgusting.
      val newline = toks"""
"""
      def `->o[->o` = flattks(toks"$newline$indentation")(toks"$newline$indentation")(newline)
      def `o_o` = flattks()(toks" ")()
      def `o,o` = flattks()(toks", ")()
      def `o_o_` = flattks()(toks" ")(toks" ") 
      def `[o,o]` = flattks(toks"[")(toks", ")(toks"]")
      def `(o,o)` = flattks(toks"(")(toks", ")(toks")")
    }
    implicit class RichTreeSeqSeq(trees: Seq[Seq[Tree]]) {
        def `(o,o)` = {
            val sq = trees match {
                case Nil => toks""
                case _ if trees.length == 1 && trees.head.length == 0 => toks"()"
                case _ => trees.flatMap(_.`(o,o)`)
            }
            Tokens(sq: _*)
        }
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
        case None => toks""
        case Some(tpe) => toks": ${tpe.tks}"
    }
    def apndTpeBounds(t: Type.Bounds): Tokens = {
        if (t.lo.isDefined || t.hi.isDefined) toks" ${t.tks}"
        else toks""
    }

    /* The helpers below are heavily based on the ones used for show[Code] */
    /* TODO: check which ones are useful in our case. Should be all (or most) of them. */

    def guessIsBackquoted(t: Name): Boolean = {
      def cantBeWrittenWithoutBackquotes(t: Name): Boolean = {
        // TODO: this requires a more thorough implementation
        // TODO: the `this` check is actually here to correctly prettyprint primary ctor calls in secondary ctors
        // this is purely an implementation artifact and will be fixed once we have tokens
        t.value != "this" && (keywords.contains(t.value) || t.value.contains(" "))
      }
      def isAmbiguousWithPatVarTerm(t: Term.Name, p: Tree): Boolean = {
        // TODO: the `eq` trick is very unreliable, but I can't come up with anything better at the moment
        // since the whole guessXXX business is going to be obsoleted by tokens very soon, I'm leaving this as is
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
    /*def guessHasRefinement(t: Type.Compound): Boolean = t.refinement.nonEmpty
    def guessPatHasRefinement(t: Pat.Type.Compound): Boolean = t.refinement.nonEmpty
    def guessIsPostfix(t: Term.Select): Boolean = false
    def guessHasExpr(t: Term.Return): Boolean = t.expr match { case Lit.Unit() => false; case _ => true }
    def guessHasElsep(t: Term.If): Boolean = t.elsep match { case Lit.Unit() => false; case _ => true }
    def guessHasStats(t: Template): Boolean = t.stats.nonEmpty
    def guessHasBraces(t: Pkg): Boolean = {
      def isOnlyChildOfOnlyChild(t: Pkg): Boolean = t.parent match {
        case Some(pkg: Pkg) => isOnlyChildOfOnlyChild(pkg) && pkg.stats.length == 1
        case Some(source: Source) => source.stats.length == 1
        case None => true
        case _ => unreachable
      }
      !isOnlyChildOfOnlyChild(t)
    }*/

    /* Infer tokens for a given tree, making use of the helpers above */
    def tkz(tree: Tree): Tokens = tree match {
        // Bottom
        case t: Quasi if t.rank > 0  => ???
        case t: Quasi if t.rank == 0 => ???

        // Name
        case t: Name.Anonymous       => toks"_"
        case t: Name.Indeterminate   =>  ???
            // TODO: this is not working, define helpers to take strings and other values in parameters.
            // This will be useful at a lot of places.
            // if (guessIsBackquoted(t)) toks"`${t.value}`"
            // else                      toks"${t.value}`"

        // Term
        // case t: Term if t.isCtorCall => ??? // TODO
        case t: Term.This            => ???
        case t: Term.Super           => ???
        case t: Term.Name            => ???
        case t: Term.Select          => ???
        case t: Term.Interpolate     => ???
        case t: Term.Apply           => ???
        case t: Term.ApplyType       => ???
        case t: Term.ApplyInfix      => ???
        case t: Term.ApplyUnary      => ???
        case t: Term.Assign          => ???
        case t: Term.Update          => ???
        case t: Term.Return          => ???
        case t: Term.Throw           => ???
        case t: Term.Ascribe         => ???
        case t: Term.Annotate        => ???
        case t: Term.Tuple           => ???
        case t: Term.Block           => ???
        case t: Term.If              => ???
        case t: Term.Match           => ???
        case t: Term.TryWithCases    => ???
        case t: Term.TryWithTerm     => ???
        case t: Term.Function        => ???
        case t: Term.PartialFunction => ???
        case t: Term.While           => ???
        case t: Term.Do              => ???
        case t: Term.For             => ???
        case t: Term.ForYield        => ???
        case t: Term.New             => ???
        case _: Term.Placeholder     => ???
        case t: Term.Eta             => ???
        case t: Term.Arg.Named       => ???
        case t: Term.Arg.Repeated    => ???
        case t: Term.Param           => ???

        // Type
        case t: Type.Name         => ???
        case t: Type.Select       => ???
        case t: Type.Project      => ???
        case t: Type.Singleton    => ???
        case t: Type.Apply        => ???
        case t: Type.ApplyInfix   => ???
        case t: Type.Function     => ???
        case t: Type.Tuple        => ???
        case t: Type.Compound     => ???
        case t: Type.Existential  => ???
        case t: Type.Annotate     => ???
        case t: Type.Placeholder  => ???
        case t: Type.Bounds       => ???
        case t: Type.Arg.Repeated => ???
        case t: Type.Arg.ByName   => ???
        case t: Type.Param        => ???

        // Pat
        case t: Pat.Var.Term         => ???
        case _: Pat.Wildcard         => ???
        case t: Pat.Bind             => ???
        case t: Pat.Alternative      => ???
        case t: Pat.Tuple            => ???
        case t: Pat.Extract          => ???
        case t: Pat.ExtractInfix     => ???
        case t: Pat.Interpolate      => ???
        case t: Pat.Typed            => ???
        case _: Pat.Arg.SeqWildcard  => ???

        // Pat.Type
        // TODO: fix copy/paste with Type
        case t: Pat.Type.Wildcard    => ???
        case t: Pat.Var.Type         => ???
        case t: Pat.Type.Project     => ???
        case t: Pat.Type.Apply       => ???
        case t: Pat.Type.ApplyInfix  => ???
        case t: Pat.Type.Function    => ???
        case t: Pat.Type.Tuple       => ???
        case t: Pat.Type.Compound    => ???
        case t: Pat.Type.Existential => ???
        case t: Pat.Type.Annotate    => ???

        // Lit
        case t: Lit.Bool if t.value => toks"true"
        case t: Lit.Bool if !t.value => toks"false"
        case t: Lit.Byte    => mineLitTk(t.value)
        case t: Lit.Short   => mineLitTk(t.value)
        case t: Lit.Int     => mineLitTk(t.value)
        case t: Lit.Long    => mineLitTk(t.value)
        case t: Lit.Float   => mineLitTk(t.value)
        case t: Lit.Double  => mineLitTk(t.value)
        case t: Lit.Char    => mineLitTk(t.value)
        case t: Lit.String  => mineLitTk(t.value)
        case t: Lit.Symbol  => mineLitTk(t.value)
        case _: Lit.Null    => toks"null"
        case _: Lit.Unit    => toks"()"

        // Member
        case t: Decl.Val       => 
            toks"${t.mods.`o_o_`}val ${t.pats.`oo`}: ${t.decltpe.tks}"
        case t: Decl.Var       =>
            toks"${t.mods.`o_o_`}var ${t.pats.`oo`}: ${t.decltpe.tks}"
        case t: Decl.Type      =>
            toks"${t.mods.`o_o_`}type ${t.name.tks}${t.tparams.`[o,o]`}${apndTpeBounds(t.bounds)}"
        case t: Decl.Def       => 
            toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}: ${t.decltpe.tks}"
        case t: Defn.Val       => 
            toks"${t.mods.`o_o_`}val ${t.pats.`oo`}${apndDeclTpe(t.decltpe)} = ${t.rhs.tks}"
        case t: Defn.Var       => 
            val rhs = t.rhs.map(trm => toks" = ${trm.tks}").getOrElse(toks"")
            toks"${t.mods.`o_o_`}var ${t.pats.`oo`}${apndDeclTpe(t.decltpe)}$rhs"
        case t: Defn.Type      =>
            toks"${t.mods.`o_o_`}type ${t.name.tks}${t.tparams.`[o,o]`} = ${t.body.tks}"
        case t: Defn.Class     => 
            toks"${t.mods.`o_o_`}class ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks}${apndTempl(t.templ)}"
        case t: Defn.Trait     =>
            toks"${t.mods.`o_o_`}trait ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks}${apndTempl(t.templ)}"
        case t: Defn.Object    => 
            toks"${t.mods.`o_o_`}object ${t.name.tks}${t.ctor.tks}${apndTempl(t.templ)}"
        case t: Defn.Def       => 
            toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}${apndDeclTpe(t.decltpe)} = ${t.body.tks}"
        case t: Defn.Macro     =>
            toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}: ${t.tpe.tks} = macro ${t.body.tks}"
        case t: Pkg            => 
            toks"package ${t.ref.tks} {${t.stats.`->o[->o`}}" // TODO: check that out, seems more complicated in show[Code]
        case t: Pkg.Object     => 
            toks"package ${t.mods.`o_o_`}object ${t.name.tks}${apndTempl(t.templ)}"
        case t: Ctor.Primary   => ???
        case t: Ctor.Secondary => ???

        // Template
        case t: Template => ???

        // Mod
        case Mod.Annot(tree)                 => ???
        case Mod.Private(Name.Anonymous())   => toks"private"
        case Mod.Private(name)               => toks"private[${name.tks}]"
        case Mod.Protected(Name.Anonymous()) => toks"protected"
        case Mod.Protected(name)             => toks"protected[${name.tks}]"
        case _: Mod.Implicit                 => toks"implicit"
        case _: Mod.Final                    => toks"final"
        case _: Mod.Sealed                   => toks"sealed"
        case _: Mod.Override                 => toks"override"
        case _: Mod.Case                     => toks"case"
        case _: Mod.Abstract                 => toks"abstract"
        case _: Mod.Covariant                => toks"+"
        case _: Mod.Contravariant            => toks"-"
        case _: Mod.Lazy                     => toks"lazy"
        case _: Mod.ValParam                 => toks"val"
        case _: Mod.VarParam                 => toks"var"
        case Mod.Ffi(signature)              => ???

        // Enumerator
        case t: Enumerator.Val           => ???
        case t: Enumerator.Generator     => ???
        case t: Enumerator.Guard         => ???

        // Import
        case t: Import.Selector.Name     => toks"${t.value.tks}"
        case t: Import.Selector.Rename   => toks"${t.from.tks} => ${t.to.tks}"
        case t: Import.Selector.Unimport => toks"${t.name.tks} => _"
        case _: Import.Selector.Wildcard => toks"_"
        case t: Import.Clause            => toks"{t.ref.tks}.${t.sels.oo}"
        case t: Import                   => toks"import ${t.clauses.`o,o`}"

        // Case
        case t: Case                     => ???

        // Source
        case t: Source                   => t.stats.`oo`
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
  private def indent(tks: Tokens)(indent: Tokens): Tokens = {
    tks // TODO
  }

  // TODO: check if required
  // Calls to the token quasiquote is not doing it so far.

  /* Put back all the positions at the righ places in a sequence of tokens. */
  private def inplace(tks: Tokens): Tokens = {
    tks // TODO
  }

}
