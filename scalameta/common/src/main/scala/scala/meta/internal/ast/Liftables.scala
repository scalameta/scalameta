package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.unreachable
import org.scalameta.adt.{LiftableMacros => AdtLiftableMacros}
import scala.meta.internal.ast.{Reflection => AstReflection}
import scala.meta.internal.ast.Metadata.Ast

// Implementation of the scala.reflect.api.Universe#Liftable interface for asts.
trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAst[T <: Ast]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

class LiftableMacros(override val c: Context) extends AdtLiftableMacros(c) with AstReflection {
  import c.universe._

  lazy val TermNameSymbol = c.mirror.staticModule("scala.meta.Term").info.member(TypeName("Name")).asClass
  lazy val CtorRefNameSymbol = c.mirror.staticModule("scala.meta.Ctor").info.member(TermName("Ref")).asModule.info.member(TypeName("Name")).asClass
  lazy val NameSymbol = c.mirror.staticClass("scala.meta.Name")
  lazy val TermApplySymbol = c.mirror.staticModule("scala.meta.Term").info.member(TypeName("Apply")).asClass
  lazy val TermSymbol = c.mirror.staticClass("scala.meta.Term")
  lazy val TermParamSymbol = c.mirror.staticModule("scala.meta.Term").info.member(TypeName("Param")).asClass
  lazy val DefnValSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Val")).asClass
  lazy val DefnVarSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Var")).asClass
  lazy val PatTypedSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Typed")).asClass
  lazy val LitSymbol = c.mirror.staticClass("scala.meta.Lit")
  lazy val TokensSymbol = c.mirror.staticClass("scala.meta.tokens.Tokens")
  lazy val DenotSymbol = c.mirror.staticClass("scala.meta.internal.semantic.Denotation")
  lazy val TypingSymbol = c.mirror.staticClass("scala.meta.internal.semantic.Typing")

  override def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiSymbol.toType))
    Some(QuasiSymbol.asBranch +: nonQuasis)
  }
  override def customWrapper(adt: Adt, defName: TermName, localName: TermName, body: Tree): Option[Tree] = {
    // TODO: it should be possible to customize liftable codegen by providing implicit instances on the outside
    // we can't just do `inferImplicitValue(adt.tpe)`, because that'll lead to a stack overflow
    // we need to do something pickling-like, but I just don't have time to implement that right now
    def reifyCoreFields(body: Tree, fields: String*): Tree = {
      // TODO: so far we don't reify core fields during pattern matching
      // because I've no idea how to do that:
      // * tokens should probably be ignored anyway
      // * denots should somehow be hygienically matched, but we don't have the technology for that
      // * typings are transient caches anyway, so probably we should ignore them, but I'm not sure
      val coreTypes = Map(
        "denot" -> DenotSymbol.toType,
        "typing" -> TypingSymbol.toType)
      val coreDefaults = Map(
        "denot" -> q"_root_.scala.meta.internal.semantic.Denotation.None",
        "typing" -> q"_root_.scala.meta.internal.semantic.Typing.None")
      def withCoreFields(body: Tree, fields: String*): Tree = {
        fields.foldLeft(body)((curr, field) => {
          val fieldSelector = q"$localName.${TermName(field)}"
          val fieldDefault = coreDefaults(field)
          val fieldLifter = q"_root_.scala.Predef.implicitly[c.universe.Liftable[${coreTypes(field)}]]"
          val withMethod = q"c.universe.Select($curr, c.universe.TermName(${"with" + field.capitalize}))"
          q"""
            if ($fieldSelector == $fieldDefault) $curr
            else c.universe.Apply($withMethod, _root_.scala.List($fieldLifter.apply($fieldSelector)))
          """
        })
      }
      q"""
        val tempBody = $body
        if (mode.isTerm) ${withCoreFields(q"tempBody", fields: _*)} else tempBody
      """
    }
    // NOTE: We have this check as a special case here, in addition to requires in Trees.scala,
    // because I think this is going to be a very common mistake that new users are going to make,
    // so I'd like that potential mistake to receive extra attention in form of quality error reporting.
    def prohibitName(pat: Tree): Tree = {
      q"""
        def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
          def unquotesName(q: _root_.scala.meta.internal.ast.Quasi): Boolean = {
            val tpe = q.hole.arg.tpe // NOTE: no easy way to find this out without holes
            tpe != null && tpe <:< typeOf[scala.meta.Term.Name]
          }
          pat match {
            case q: _root_.scala.meta.internal.ast.Quasi if unquotesName(q) =>
              val action = if (q.rank == 0) "unquote" else "splice"
              c.abort(q.pos, "can't " + action + " a name here, use a pattern instead")
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
          def unapply(tree: _root_.scala.meta.Tree): Option[(_root_.scala.meta.Term, _root_.scala.meta.Term.Arg.Quasi)] = tree match {
            case _root_.scala.meta.Term.Apply(fn, _root_.scala.collection.immutable.Seq(arg: _root_.scala.meta.Term.Arg.Quasi))
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
              ${liftPath("scala.meta.internal.ast.Syntactic.Term.Apply")},
              _root_.scala.collection.immutable.List(
                ${liftField(q"fn", tq"_root_.scala.meta.Term")},
                ${liftField(q"Seq(Seq(tripleQuasi))", tq"Seq[Seq[_root_.scala.meta.Term.Arg.Quasi]]")}))
          case _ =>
            $body
        }
      """
    }
    def customize(body: Tree): Option[Tree] = {
      if (adt.tpe <:< QuasiSymbol.toType) Some(q"Lifts.liftQuasi($localName)")
      else if (adt.tpe <:< TermNameSymbol.toType) Some(reifyCoreFields(body, "denot", "typing"))
      else if (adt.tpe <:< CtorRefNameSymbol.toType) Some(reifyCoreFields(body, "denot", "typing"))
      else if (adt.tpe <:< NameSymbol.toType) Some(reifyCoreFields(body, "denot"))
      else if (adt.tpe <:< TermApplySymbol.toType) Some(reifyCoreFields(specialcaseTermApply(body), "typing"))
      else if (adt.tpe <:< TermSymbol.toType) Some(reifyCoreFields(body, "typing"))
      else if (adt.tpe <:< TermParamSymbol.toType) Some(reifyCoreFields(body, "typing"))
      else if (adt.tpe <:< DefnValSymbol.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitName(q"pat")}); $body }")
      else if (adt.tpe <:< DefnVarSymbol.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitName(q"pat")}); $body }")
      else if (adt.tpe <:< PatTypedSymbol.toType) Some(q"{ ${prohibitName(q"$localName.lhs")}; $body }")
      else None
    }
    // NOTE: we ignore tokens here for the time being
    val body1 = {
      if (adt.tpe <:< LitSymbol.toType) {
        q"""
          implicit object LiftableAny extends c.universe.Liftable[_root_.scala.Any] {
            def apply(value: _root_.scala.Any): c.universe.Tree = value match {
              case value: _root_.scala.Boolean => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Boolean]].apply(value)
              case value: _root_.scala.Byte => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Byte]].apply(value)
              case value: _root_.scala.Short => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Short]].apply(value)
              case value: _root_.scala.Int => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Int]].apply(value)
              case value: _root_.scala.Long => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Long]].apply(value)
              case value: _root_.scala.Float => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Float]].apply(value)
              case value: _root_.scala.Double => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Double]].apply(value)
              case value: _root_.scala.Char => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Char]].apply(value)
              case value: _root_.scala.Predef.String => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Predef.String]].apply(value)
              case value: _root_.scala.Symbol => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Symbol]].apply(value)
              case null => c.universe.Literal(c.universe.Constant(null))
              case value: _root_.scala.Unit => _root_.scala.Predef.implicitly[c.universe.Liftable[_root_.scala.Unit]].apply(value)
            }
          }
          $body
        """
      } else {
        body
      }
    }
    customize(body1)
  }
}