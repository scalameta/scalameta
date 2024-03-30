package scala.meta
package internal
package trees

import org.scalameta.adt.{LiftableMacros => AdtLiftableMacros}
import scala.meta.internal.trees.Metadata.Ast
import scala.meta.internal.trees.{Reflection => AstReflection}

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context

// Implementation of the scala.reflect.api.Universe#Liftable interface for asts.
trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAst[T <: Ast](isPrivateOKExpr: Boolean): u.Liftable[T] =
    macro LiftableMacros.impl[T]
}

class LiftableMacros(override val c: Context) extends AdtLiftableMacros(c) with AstReflection {
  import c.universe._

  lazy val TermApplySymbol = c.mirror.staticModule("scala.meta.Term").info.member(TypeName("Apply"))
    .asClass
  lazy val DefnValSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Val"))
    .asClass
  lazy val DefnVarSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Var"))
    .asClass
  lazy val PatBindSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Bind"))
    .asClass
  lazy val PatTypedSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Typed"))
    .asClass
  lazy val LitSymbol = c.mirror.staticClass("scala.meta.Lit")
  lazy val TokensSymbol = c.mirror.staticClass("scala.meta.tokens.Tokens")

  override def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiSymbol.toType))
    Some(QuasiSymbol.asBranch +: nonQuasis)
  }
  override def customWrapper(
      adt: Adt,
      defName: TermName,
      localName: TermName,
      body: Tree
  ): Option[Tree] = {
    // NOTE: See #277 and #405 to understand why this special-casing is necessary.
    def specialcaseTermApply: Tree = q"""
        object ApplyToTripleDots {
          def unapply(t: _root_.scala.meta.Term.Apply): Option[(
            _root_.scala.meta.Term,
            _root_.scala.meta.Term.ArgClause.Quasi
          )] = t.argClause match {
            case arg: _root_.scala.meta.Term.ArgClause.Quasi if arg.rank == 1 =>
              _root_.scala.Some((t.fun, arg))
            case _ => _root_.scala.None
          }
        }
        @tailrec
        private def checkNoTripleDots(
          fn: _root_.scala.meta.Term,
          arg: _root_.scala.meta.internal.trees.Quasi
        ): Unit = fn match {
          case t: _root_.scala.meta.Term.Apply =>
            ApplyToTripleDots.unapply(t) match {
              case _root_.scala.None => checkNoTripleDots(t.fun, arg)
              case _ => c.abort(arg.pos,
                _root_.scala.meta.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(arg.rank))
            }
          case _ => // do nothing
        }
        private def applyArgClauseQuasi(fn: _root_.scala.meta.Term)(arg: _root_.scala.meta.Term.ArgClause.Quasi) = {
          checkNoTripleDots(fn, arg)
          c.universe.Apply(
            ${liftPath("scala.meta.internal.trees.Syntactic.TermApply.ArgList")},
            _root_.scala.List(
              ${liftField(q"fn", tq"_root_.scala.meta.Term")},
              ${liftField(q"List(arg)", tq"List[_root_.scala.meta.Term.ArgClause.Quasi]")}
            )
          )
        }
        $localName match {
          case ApplyToTripleDots(fn, acq) =>
            applyArgClauseQuasi(fn)(acq)
          case _ =>
            $body
        }
      """
    // NOTE: we ignore tokens here for the time being
    if (adt.tpe <:< QuasiSymbol.toType) Some(q"Lifts.liftQuasi($localName)")
    else if (adt.tpe <:< TermApplySymbol.toType) Some(specialcaseTermApply)
    else if (adt.tpe <:< DefnValSymbol.toType)
      Some(q"{ $localName.pats.foreach(pat => ${prohibitName(q"pat")}); $body }")
    else if (adt.tpe <:< DefnVarSymbol.toType)
      Some(q"{ $localName.pats.foreach(pat => ${prohibitName(q"pat")}); $body }")
    else if (adt.tpe <:< PatBindSymbol.toType)
      Some(q"{ ${prohibitName(q"$localName.lhs")}; $body }")
    else if (adt.tpe <:< PatTypedSymbol.toType)
      Some(q"{ ${prohibitName(q"$localName.lhs")}; $body }")
    else None
  }

  // NOTE: We have this check as a special case here, in addition to requires in Trees.scala,
  // because I think this is going to be a very common mistake that new users are going to make,
  // so I'd like that potential mistake to receive extra attention in form of quality error reporting.
  private def prohibitName(pat: Tree): Tree = q"""
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

  private def liftPath(path: String): Tree = {
    val init = q"""c.universe.Ident(c.universe.TermName("_root_"))""": Tree
    path.split('.').foldLeft(init) { (acc, part) =>
      q"c.universe.Select($acc, c.universe.TermName($part))"
    }
  }
  private def liftField(value: Tree, tpe: Tree): Tree =
    q"_root_.scala.Predef.implicitly[c.universe.Liftable[$tpe]].apply($value)"
}
