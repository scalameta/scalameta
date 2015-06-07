package org.scalameta.ast

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.unreachable
import org.scalameta.ast.internal.Ast
import org.scalameta.adt.{LiftableMacros => AdtLiftableMacros}
import org.scalameta.ast.{Reflection => AstReflection}

trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAst[T <: Ast]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

class LiftableMacros(override val c: Context) extends AdtLiftableMacros(c) with AstReflection {
  import c.universe._
  lazy val QuasiClass = c.mirror.staticClass("scala.meta.internal.ast.Quasi")
  lazy val TermNameClass = c.mirror.staticModule("scala.meta.internal.ast.Term").info.member(TypeName("Name")).asClass
  lazy val CtorRefNameClass = c.mirror.staticModule("scala.meta.internal.ast.Ctor").info.member(TermName("Ref")).asModule.info.member(TypeName("Name")).asClass
  lazy val NameClass = c.mirror.staticClass("scala.meta.internal.ast.Name")
  lazy val TermClass = c.mirror.staticClass("scala.meta.internal.ast.Term")
  lazy val DefnValClass = c.mirror.staticModule("scala.meta.internal.ast.Defn").info.member(TypeName("Val")).asClass
  lazy val DefnVarClass = c.mirror.staticModule("scala.meta.internal.ast.Defn").info.member(TypeName("Var")).asClass
  lazy val PatTypedClass = c.mirror.staticModule("scala.meta.internal.ast.Pat").info.member(TypeName("Typed")).asClass
  override def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiClass.toType))
    Some(QuasiClass.asBranch +: nonQuasis)
  }
  override def customWrapper(adt: Adt, defName: TermName, localName: TermName, body: Tree): Option[Tree] = {
    // TODO: it should be possible to customize liftable codegen by providing implicit instances on the outside
    // we can't just do `inferImplicitValue(adt.tpe)`, because that'll lead to a stack overflow
    // we need to do something pickling-like, but I just don't have time to implement that right now
    def reifyAuxiliaryFields(body: Tree, fields: String*): Tree = {
      // TODO: so far we don't reify auxiliary fields during pattern matching
      // because I've no idea how to do that:
      // * denots and sigmas should somehow be hygienically matched, but we don't have the technology for that
      // * statuses and expansions are transient caches anyway, so probably we should ignore them, but I'm not sure
      def copyAuxiliaryFields(body: Tree, fields: String*): Tree = {
        def argName(field: String) = q"c.universe.Ident(c.universe.TermName($field))"
        def argValue(field: String) = {
          val fieldTpe = adt.asInstanceOf[Leaf].allFields.find(_.name.toString == field).map(_.tpe).get
          q"_root_.scala.Predef.implicitly[c.universe.Liftable[$fieldTpe]].apply($localName.${TermName(field)})"
        }
        val copyArgs = fields.map(field => q"c.universe.AssignOrNamedArg(${argName(field)}, ${argValue(field)})").toList
        q"""c.universe.Apply(c.universe.Select($body, c.universe.TermName("copy")), _root_.scala.List(..$copyArgs))"""
      }
      q"""
        val tempBody = $body
        if (mode.isTerm) ${copyAuxiliaryFields(q"tempBody", fields: _*)} else tempBody
      """
    }
    def prohibitLowercasePat(pat: Tree): Tree = {
      q"""
        def prohibitLowercasePat(pat: scala.meta.Tree): _root_.scala.Unit = {
          pat match {
            case q: scala.meta.internal.ast.Quasi
            if q.tree.asInstanceOf[c.universe.Tree].tpe <:< typeOf[scala.meta.Term.Name] =>
              val action = if (q.rank == 0) "unquote" else "splice"
              c.abort(q.position, "can't " + action + " a name here, use a variable pattern instead")
            case _ =>
          }
        }
        prohibitLowercasePat($pat)
      """
    }
    if (adt.tpe <:< QuasiClass.toType) Some(q"Lifts.liftQuasi($localName)")
    else if (adt.tpe <:< TermNameClass.toType) Some(reifyAuxiliaryFields(body, "denot", "sigma", "status", "expansion"))
    else if (adt.tpe <:< CtorRefNameClass.toType) Some(reifyAuxiliaryFields(body, "denot", "sigma", "status", "expansion"))
    else if (adt.tpe <:< NameClass.toType) Some(reifyAuxiliaryFields(body, "denot", "sigma"))
    else if (adt.tpe <:< TermClass.toType) Some(reifyAuxiliaryFields(body, "status", "expansion"))
    else if (adt.tpe <:< DefnValClass.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitLowercasePat(q"pat")}); $body }")
    else if (adt.tpe <:< DefnVarClass.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitLowercasePat(q"pat")}); $body }")
    else if (adt.tpe <:< PatTypedClass.toType) Some(q"{ ${prohibitLowercasePat(q"$localName.lhs")}; $body }")
    else None
  }
}