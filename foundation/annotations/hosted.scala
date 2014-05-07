package org.scalareflect.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class hosted(macroApi: Boolean = false, mayFail: Boolean = true) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro HostedMacros.impl
}

class HostedMacros(val c: Context) {
  import c.universe._
  def impl(annottees: c.Tree*): c.Tree = {
    val args = c.macroApplication match {
      case q"new $_(..$args).macroTransform(..$_)" => args
      case q"new $_().macroTransform(..$_)" => Nil
    }
    val macroApi = args.collect{ case q"macroApi = true" => true }.nonEmpty
    val mayFail = args.collect{ case q"mayFail = false" => true }.isEmpty
    val exnTpe = tq"_root_.scala.reflect.core.ReflectionException"
    val contextTpe = if (macroApi) tq"_root_.scala.reflect.semantic.MacroContext" else tq"_root_.scala.reflect.semantic.HostContext"
    val failWrapper = if (macroApi) q"_root_.scala.reflect.semantic.errors.wrapMacrohosted" else q"_root_.scala.reflect.semantic.errors.wrapHosted"
    def transform(ddef: DefDef): DefDef = {
      val DefDef(mods, name, tparams, vparamss, tpt, body) = ddef
      val mayFails = if (mayFail) List(q"new _root_.org.scalareflect.errors.mayFail[$exnTpe]") else Nil
      val contextful = q"new _root_.org.scalareflect.annotations.contextful[$contextTpe]"
      val footprint = q"new _root_.org.scalareflect.annotations.internal.hosted(macroApi = $macroApi, mayFail = $mayFail)"
      val mods1 = Modifiers(mods.flags, mods.privateWithin, mods.annotations ++ mayFails ++ List(contextful, footprint))
      val autoBody = body match { case q"delegate" => true; case _ => false }
      val body1 = if (autoBody) {
        val owner = c.internal.enclosingOwner
        val isInPackageObject = (owner.isModuleClass && owner.name == typeNames.PACKAGE) || (owner.name.toString.endsWith("Ops"))
        val isInImplicitClass = owner.isClass && owner.isImplicit
        def paramRef(p: ValDef) = {
          val isVararg = p.tpt match {
            case tq"$_.$name[..$_]" if name == definitions.RepeatedParamClass.name => true
            case _ => false
          }
          if (isVararg) q"${p.name}: _*" else q"${p.name}"
        }
        var args = vparamss.map(_.map(paramRef))
        def prependArg(arg: Tree): Unit = {
          args = args match {
            case hd :: tl => (arg +: hd) :: tl
            case Nil => List(List(arg))
          }
        }
        if (!isInPackageObject && !isInImplicitClass && !macroApi) prependArg(q"this")
        if (isInImplicitClass && !macroApi) prependArg(q"tree")
        if (mayFail) q"$failWrapper(_.$name(...$args))"
        else q"implicitly[$contextTpe].$name(...$args)"
      } else body
      DefDef(mods1, name, tparams, vparamss, tpt, body1)
    }
    val expanded = annottees match {
      case (ddef: DefDef) :: rest => transform(ddef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only methods can be @hosted")
    }
    q"{ ..$expanded; () }"
  }
}
