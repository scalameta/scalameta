package scala.meta
package internal
package ui

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


  /* Generate synthetic tokens */
  private def infer(tree: Tree)(implicit dialect: Dialect): Tokens = {
    import scala.meta.internal.ast._
    import scala.meta.dialects.Scala211 // TODO: figure out why the implicit in params is not enough
  
    implicit def toTokenSeq(tk: Token) = Tokens(tk)
  
    implicit class RichTree(tree: Tree) {
      def tks = ident(tree.tokens)("  ") // TODO: figure out how to find proper ident string
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
      def oo = flattks()()()
      def o_o = flattks()(toks" ")()
      def o_o_ = flattks()(toks" ")(toks" ") 
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

    def tkz(tree: Tree): Tokens = tree match {
        // Bottom
        case t: Quasi if t.rank > 0  => ???
        case t: Quasi if t.rank == 0 => ???

        // Name
        case t: Name.Anonymous       => ???
        case t: Name.Indeterminate   => ???

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
        case t: Lit.Bool    => ???
        case t: Lit.Byte    => ???
        case t: Lit.Short   => ???
        case t: Lit.Int     => ???
        case t: Lit.Long    => ???
        case t: Lit.Float   => ???
        case t: Lit.Double  => ???
        case t: Lit.Char    => ???
        case t: Lit.String  => ???
        case t: Lit.Symbol  => ???
        case _: Lit.Null    => ???
        case _: Lit.Unit    => ???

        // Member
        case t: Decl.Val       => ???
        case t: Decl.Var       => ???
        case t: Decl.Type      => ???
        case t: Decl.Def       => ???
        case t: Defn.Val       => ???
        case t: Defn.Var       => ???
        case t: Defn.Type      => ???
        case t: Defn.Class     => 
            val ext = if (!t.templ.parents.isEmpty) toks"extends " else toks""
            toks"${t.mods.o_o_}class ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks} $ext${t.templ.tks}"
        case t: Defn.Trait     => ???
        case t: Defn.Object    => 
            val ext = if (!t.templ.parents.isEmpty) toks"extends " else toks""
            toks"${t.mods.o_o_}object ${t.name.tks}${t.ctor.tks} $ext${t.templ.tks}"
        case t: Defn.Def       => 
            val tpe = t.decltpe match {
                case None => toks""
                case Some(tpe) => toks": ${tpe.tks}"
            }
            toks"${t.mods.o_o_}def ${t.name.tks}${t.tparams.`[o,o]`}${t.paramss.`(o,o)`}$tpe = ${t.body.tks}"
        case t: Defn.Macro     => ???
        case t: Pkg            => ???
        case t: Pkg.Object     => ???
        case t: Ctor.Primary   => ???
        case t: Ctor.Secondary => ???

        // Template
        case t: Template => ???

        // Mod
        case Mod.Annot(tree)                 => ???
        case Mod.Private(Name.Anonymous())   => ???
        case Mod.Private(name)               => ???
        case Mod.Protected(Name.Anonymous()) => ???
        case Mod.Protected(name)             => ???
        case _: Mod.Implicit                 => ???
        case _: Mod.Final                    => ???
        case _: Mod.Sealed                   => ???
        case _: Mod.Override                 => ???
        case _: Mod.Case                     => ???
        case _: Mod.Abstract                 => ???
        case _: Mod.Covariant                => ???
        case _: Mod.Contravariant            => ???
        case _: Mod.Lazy                     => ???
        case _: Mod.ValParam                 => ???
        case _: Mod.VarParam                 => ???
        case Mod.Ffi(signature)              => ???

        // Enumerator
        case t: Enumerator.Val           => ???
        case t: Enumerator.Generator     => ???
        case t: Enumerator.Guard         => ???

        // Import
        case t: Import.Selector.Name     => ???
        case t: Import.Selector.Rename   => ???
        case t: Import.Selector.Unimport => ???
        case _: Import.Selector.Wildcard => ???
        case t: Import.Clause            => ???
        case t: Import                   => ???

        // Case
        case t: Case                     => ???

        // Source
        case t: Source                   => t.stats.oo
    }
    tkz(tree.asInstanceOf[scala.meta.internal.ast.Tree])
  }

  /* Adding proper identation to the token stream */
  private def ident(tks: Tokens)(implicit ident: String): Tokens = {
    tks // TODO
  }

}
