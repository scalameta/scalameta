package scala.meta
package internal
package trees

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.adt.{LiftableMacros => AdtLiftableMacros}
import scala.meta.internal.trees.{Reflection => AstReflection}
import scala.meta.internal.trees.Metadata.Ast

// Implementation of the scala.reflect.api.Universe#Liftable interface for asts.
trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAst[T <: Ast]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

class LiftableMacros(override val c: Context) extends AdtLiftableMacros(c) with AstReflection {
  import c.universe._

  lazy val TermApplySymbol = c.mirror.staticModule("scala.meta.Term").info.member(TypeName("Apply")).asClass
  lazy val DefnValSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Val")).asClass
  lazy val DefnVarSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Var")).asClass
  lazy val PatBindSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Bind")).asClass
  lazy val PatTypedSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Typed")).asClass
  lazy val LitSymbol = c.mirror.staticClass("scala.meta.Lit")
  lazy val TokensSymbol = c.mirror.staticClass("scala.meta.tokens.Tokens")

  override def customAdts(root: Root): Option[List[Adt]] = {
    var nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiSymbol.toType))
    Some(QuasiSymbol.asBranch +: nonQuasis)
  }
  override def customWrapper(adt: Adt, defName: TermName, localName: TermName, body: Tree): Option[Tree] = {
    // NOTE: We have this check as a special case here, in addition to requires in Trees.scala,
    // because I think this is going to be a very common mistake that new users are going to make,
    // so I'd like that potential mistake to receive extra attention in form of quality error reporting.
    def prohibitName(pat: Tree): Tree = {
      q"""
        def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
          def unquotesName(q: _root_.scala.meta.internal.trees.Quasi): Boolean = {
            val tpe = q.hole.arg.tpe // NOTE: no easy way to find this out without holes
            tpe != null && tpe <:< typeOf[scala.meta.Term.Name]
          }
          pat match {
            case q: _root_.scala.meta.internal.trees.Quasi if unquotesName(q) =>
              val action = if (q.rank == 0) "unquote" else "splice"
              c.abort(q.pos, "can't " + action + " a name here, use a pattern instead (e.g. p\"x\")")
            case _ =>
          }
        }
        prohibitName($pat)
      """
    }
    // NOTE: See #277 and #405 to understand why this special-casing is necessary.
    def specialcaseTermApply(body: Tree): Tree = {
      def liftPath(path: String) = {
        val init = q"""c.universe.Ident(c.universe.TermName("_root_"))""": Tree
        path.split('.').foldLeft(init)((acc, part) => q"c.universe.Select($acc, c.universe.TermName($part))")
      }
      def liftField(value: Tree, tpe: Tree) = {
        q"_root_.scala.Predef.implicitly[c.universe.Liftable[$tpe]].apply($value)"
      }
      q"""
        object ApplyToTripleDots {
          def unapply(tree: _root_.scala.meta.Tree): Option[(_root_.scala.meta.Term, _root_.scala.meta.Term.Quasi)] = tree match {
            case _root_.scala.meta.Term.Apply(fn, _root_.scala.collection.immutable.List(arg: _root_.scala.meta.Term.Quasi))
            if arg.rank == 2 => _root_.scala.Some((fn, arg))
            case _ => _root_.scala.None
          }
        }
        $localName match {
          case ApplyToTripleDots(fn, tripleQuasi) =>
            def checkNoTripleDots(fn: _root_.scala.meta.Term): Unit = fn match {
              case ApplyToTripleDots(fn, previousTripleQuasi) => c.abort(tripleQuasi.pos, _root_.scala.meta.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(tripleQuasi.rank))
              case _root_.scala.meta.Term.Apply(fn, _) => checkNoTripleDots(fn)
              case _ => // do nothing
            }
            checkNoTripleDots(fn)

            c.universe.Apply(
              ${liftPath("scala.meta.internal.trees.Syntactic.Term.Apply")},
              _root_.scala.collection.immutable.List(
                ${liftField(q"fn", tq"_root_.scala.meta.Term")},
                ${liftField(q"List(List(tripleQuasi))", tq"List[List[_root_.scala.meta.Term.Quasi]]")}))
          case _ =>
            $body
        }
      """
    }
    def customize(body: Tree): Option[Tree] = {
      if (adt.tpe <:< QuasiSymbol.toType) Some(q"Lifts.liftQuasi($localName)")
      else if (adt.tpe <:< TermApplySymbol.toType) Some(specialcaseTermApply(body))
      else if (adt.tpe <:< DefnValSymbol.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitName(q"pat")}); $body }")
      else if (adt.tpe <:< DefnVarSymbol.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitName(q"pat")}); $body }")
      else if (adt.tpe <:< PatBindSymbol.toType) Some(q"{ ${prohibitName(q"$localName.lhs")}; $body }")
      else if (adt.tpe <:< PatTypedSymbol.toType) Some(q"{ ${prohibitName(q"$localName.lhs")}; $body }")
      else None
    }
    // NOTE: we ignore tokens here for the time being
    customize(body)
  }
}
