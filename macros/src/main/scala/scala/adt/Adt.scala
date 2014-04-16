package scala.adt

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.invariants.nonEmpty
import scala.reflect.macros.whitebox.Context

class adt extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.adt
}

class leaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.leaf
}

class AdtMacros(val c: Context) {
  import c.universe._
  import Flag._

  def adt(annottees: c.Tree*): c.Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, impl) = cdef
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @adt traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@adt traits cannot be final")
      val flags1 = flags | SEALED
      ClassDef(Modifiers(flags1, privateWithin, anns), name, tparams, impl)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @adt")
    }
    q"{ ..$expanded; () }"
  }

  // 4) withXXX
  // 5) def tag: Int = ...
  // 6) null checks
  // 7) @NonEmpty checks
  // 8) deep immutability check (via def macros)
  // 9) deep sealedness check (via def macros as well)
  def leaf(annottees: c.Tree*): c.Tree = {
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