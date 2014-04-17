package org.scalareflect.adt

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import org.scalareflect.invariants.nonEmpty
import scala.reflect.macros.whitebox.Context

// (Eugene) TODO: think what this can mean, e.g. ensures that trees stemming from roots are disjoint
class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.branch
}

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.branch
}

class leaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.leaf
}

class AdtMacros(val c: Context) {
  import c.universe._
  import Flag._

  // (Eugene) TODO: check transitive sealedness
  // (Eugene) TODO: check rootness
  def branch(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, impl) = cdef
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @branch traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags | SEALED
      ClassDef(Modifiers(flags1, privateWithin, anns), name, tparams, impl)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @branch")
    }
    q"{ ..$expanded; () }"
  }

  // (Eugene) TODO: withXXX
  // (Eugene) TODO: def tag: Int = ...
  // (Eugene) TODO: null checks
  // (Eugene) TODO: @NonEmpty checks
  // (Eugene) TODO: deep immutability check (via def macros)
  // (Eugene) TODO: deep sealedness check (via def macros as well)
  // (Eugene) TODO: check rootness
  def leaf(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"${mods @ Modifiers(flags, privateWithin, anns)} class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @leaf classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @leaf classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @leaf classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@leaf classes cannot be abstract")
      if (paramss.length != 1) c.abort(cdef.pos, "@leaf classes must have one parameter list")
      val params = paramss.head
      params.foreach(p => if (p.mods.hasFlag(MUTABLE)) c.abort(p.pos, "@leaf classes must be immutable"))
      val flags1 = flags | CASE | FINAL
      val params1 = params.map{ case q"$mods val $name: $tpt = $default" => q"val $name: $tpt = $default" }
      List(q"${Modifiers(flags1, privateWithin, anns)} class $name[..$tparams] $ctorMods(..$params1) extends { ..$earlydefns } with ..$parents { $self => ..$stats }", mdef)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transform(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transform(cdef, q"object ${name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only classes can be @leaf")
    }
    q"{ ..$expanded; () }"
  }
}
