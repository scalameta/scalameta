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
  lazy val TermParamClass = c.mirror.staticModule("scala.meta.internal.ast.Term").info.member(TypeName("Param")).asClass
  lazy val DefnValClass = c.mirror.staticModule("scala.meta.internal.ast.Defn").info.member(TypeName("Val")).asClass
  lazy val DefnVarClass = c.mirror.staticModule("scala.meta.internal.ast.Defn").info.member(TypeName("Var")).asClass
  lazy val PatTypedClass = c.mirror.staticModule("scala.meta.internal.ast.Pat").info.member(TypeName("Typed")).asClass
  lazy val TokensClass = c.mirror.staticClass("scala.meta.syntactic.Tokens")
  lazy val DenotClass = c.mirror.staticClass("scala.meta.internal.semantic.Denotation")
  lazy val TypingClass = c.mirror.staticClass("scala.meta.internal.semantic.Typing")
  lazy val ExpansionClass = c.mirror.staticClass("scala.meta.internal.semantic.Expansion")
  override def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiClass.toType))
    Some(QuasiClass.asBranch +: nonQuasis)
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
      // * typings and expansions are transient caches anyway, so probably we should ignore them, but I'm not sure
      val coreTypes = Map(
        "denot" -> DenotClass.toType,
        "typing" -> TypingClass.toType,
        "expansion" -> ExpansionClass.toType)
      val coreDefaults = Map(
        "denot" -> q"_root_.scala.meta.internal.semantic.Denotation.Zero",
        "typing" -> q"_root_.scala.meta.internal.semantic.Typing.Unknown",
        "expansion" -> q"_root_.scala.meta.internal.semantic.Expansion.Identity")
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
    // NOTE: we ignore tokens here for the time being
    if (adt.tpe <:< QuasiClass.toType) Some(q"Lifts.liftQuasi($localName)")
    else if (adt.tpe <:< TermNameClass.toType) Some(reifyCoreFields(body, "denot", "typing", "expansion"))
    else if (adt.tpe <:< CtorRefNameClass.toType) Some(reifyCoreFields(body, "denot", "typing", "expansion"))
    else if (adt.tpe <:< NameClass.toType) Some(reifyCoreFields(body, "denot"))
    else if (adt.tpe <:< TermClass.toType) Some(reifyCoreFields(body, "typing", "expansion"))
    else if (adt.tpe <:< TermParamClass.toType) Some(reifyCoreFields(body, "typing"))
    else if (adt.tpe <:< DefnValClass.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitLowercasePat(q"pat")}); $body }")
    else if (adt.tpe <:< DefnVarClass.toType) Some(q"{ $localName.pats.foreach(pat => ${prohibitLowercasePat(q"pat")}); $body }")
    else if (adt.tpe <:< PatTypedClass.toType) Some(q"{ ${prohibitLowercasePat(q"$localName.lhs")}; $body }")
    else None
  }
}