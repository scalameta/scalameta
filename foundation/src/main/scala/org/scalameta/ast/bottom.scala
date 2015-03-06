package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

class bottom extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BottomMacros.impl
}

class BottomMacros(val c: Context) {
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  val AstInternal = q"_root_.org.scalameta.ast.internal"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val ClassDef(mods, name, tparams, Template(parents, self, stats)) = cdef
      val enclosingUnit = c.parse(new String(c.internal.enclosingOwner.pos.source.content))
      object astClassDetector extends Traverser {
        val astClasses = ListBuffer[Tree]()
        val astFields = ListBuffer[TermName](TermName("denot"), TermName("sigma"))
        var module = q"_root_": Tree
        var inner = false
        def drilldown[T](moduleName: String, inner: Boolean)(op: => T) = {
          val savedModule = this.module
          val savedInner = this.inner
          this.module = c.parse(module.toString + "." + moduleName)
          this.inner = inner
          val result = op
          this.inner = savedInner
          this.module = savedModule
          result
        }
        override def traverse(tree: Tree): Unit = tree match {
          case PackageDef(pid, stats) =>
            drilldown(pid.toString, inner = false)(super.traverse(tree))
          case ModuleDef(_, name, _) =>
            drilldown(name.toString, inner = false)(super.traverse(tree))
          case ClassDef(Modifiers(_, _, anns), name, _, impl) =>
            if (inner) c.abort(tree.pos, "@ast classes can't be inner")
            if (anns.exists(_.toString == "new ast()") && !anns.exists(_.toString == "new bottom()")) {
              astClasses += Select(module, name)
              val q"$_ class $_[..$_] $_(...$paramss) extends { ..$_ } with ..$_ { $_ => ..$_ }" = tree
              astFields ++= paramss.flatten.map(_.name)
            }
            drilldown(name.toString, inner = true)(super.traverse(tree))
          case _ =>
            super.traverse(tree)
        }
      }
      astClassDetector.traverse(enclosingUnit)
      val mods1 = Modifiers(mods.flags, TypeName("meta"), mods.annotations)
      val parents1 = parents ++ astClassDetector.astClasses
      val stats1 = stats ++ astClassDetector.astFields.distinct.map(name => {
        val message = s"unsupported ${if (name.toString == "Unquote") "unquoting" else "splicing"} position"
        val impl = q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
        q"override def $name: _root_.scala.Nothing = $impl"
      })
      ClassDef(mods1, name, tparams, Template(parents1, self, stats1))
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if !mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes can be @bottom")
    }
    q"{ ..$expanded; () }"
  }
}