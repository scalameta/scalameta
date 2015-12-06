package org.scalameta

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import org.scalameta.roles.macros._
import org.scalameta.invariants._

package object roles {
  implicit class XtensionRole[U](x: U) {
    // TODO #1: We can't have `object TermLoc extends Role[Tree] with Enroller[Tree, TermLoc.type]`,
    // because scalac spuriously turns this into a cyclic reference error.
    // Interestingly enough, this is fixed in Dotty.

    def is[T, R, E](e: E)(implicit evCanEnroll: CanEnroll[T, R, E], evSub: <:<[U, T]): Boolean = {
      evCanEnroll.e.isDefined(x)
    }

    // overload one for the case of @role class X
    def set[T, R, E](r: R)(implicit evCanEnroll: CanEnroll[T, R, E], evSub: <:<[U, T]): U = {
      evCanEnroll.e.set(x.asInstanceOf[T], r).asInstanceOf[U]
    }

    // overload two for the case of @role object y
    def set[T, E <: Enroller[_, _]](e: E)(implicit evCanEnroll: CanEnroll[T, internal.Dummy.type, E], evSub: <:<[U, T]): U = {
      evCanEnroll.e.set(x.asInstanceOf[T], internal.Dummy).asInstanceOf[U]
    }

    def get[T, R, E](e: E)(implicit evCanEnroll: CanEnroll[T, R, E], evSub: <:<[U, T]): R = {
      evCanEnroll.e.get(x).get
    }
  }

  class role extends StaticAnnotation {
    import scala.language.experimental.macros
    def macroTransform(annottees: Any*): Any = macro RoleMacros.role
  }

  trait Enroller[T, R] {
    def isDefined(x: T): Boolean
    def get(x: T): Option[R]
    def set[U <: T](x: U, r: R): U
  }

  trait CanEnroll[T, R, E] {
    def e: Enroller[T, R]
  }
}

package roles.internal {
  trait Role
  object Dummy
}

package roles.macros {
  class RoleMacros(val c: Context) {
    import c.universe._
    import definitions._
    import Flag._

    def role(annottees: Tree*): Tree = {
      def extractParents[T <: ImplDef](owner: T): (List[Tree], Boolean, Tree) = {
        val parents = owner.impl.parents
        var isReadonly: Option[Boolean] = None
        var tpe: Option[Tree] = None
        val parents1 = parents.map {
          case parent @ AppliedTypeTree(Ident(TypeName("Role")), args @ List(arg)) =>
            if (isReadonly.nonEmpty) c.abort(parent.pos, "mustn't extend Role twice")
            isReadonly = Some(false)
            tpe = Some(arg)
            tq"_root_.org.scalameta.roles.internal.Role"
          case parent @ AppliedTypeTree(Ident(TypeName("ReadonlyRole")), args @ List(arg)) =>
            if (isReadonly.nonEmpty) c.abort(parent.pos, "mustn't extend Role twice")
            isReadonly = Some(true)
            tpe = Some(arg)
            tq"_root_.org.scalameta.roles.internal.Role"
          case parent =>
            parent
        }
        (parents1, isReadonly.getOrElse(c.abort(owner.pos, "must extend Role or ReadonlyRole")), tpe.get)
      }

      def casify(mods: Modifiers): Modifiers = {
        val Modifiers(flags, privateWithin, anns) = mods
        Modifiers(flags | CASE, privateWithin, anns)
      }

      def unprivatize(mods: Modifiers) = {
        val Modifiers(flags, privateWithin, anns) = mods
        val flags1 = (flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.LOCAL) & (~scala.reflect.internal.Flags.PRIVATE)).asInstanceOf[FlagSet]
        Modifiers(flags1, privateWithin, anns)
      }

      def transformRoleClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
        val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
        val ModuleDef(mmods, mname, mtempl @ Template(mparents, mself, mstats)) = mdef
        val (parents1, isReadonly, tpe) = extractParents(cdef)
        val parents2 = parents1 :+ tq"_root_.scala.Product"
        val mods1 = casify(mods)
        val paramss1 = paramss.map(_.map({
          case q"$mods val $name: $tpt = $default" =>
            q"${unprivatize(mods)} val $name: $tpt = $default"
        }))
        val T = tpe
        val R = Ident(name)
        val E = tq"${Ident(mname)}.type"
        val mparents1 = mparents :+ tq"_root_.org.scalameta.roles.Enroller[$T, $R]"
        val defIsDefined = q"def isDefined(x: $T): Boolean = this.get(x).isDefined"
        val defSet = {
          if (isReadonly) q"def set[U <: $T](x: U, r: $R): U = throw new UnsupportedOperationException"
          else q"/* empty tree */"
        }
        val q"..$instanceCanEnroll" = q"""
          private object InternalCanEnroll extends _root_.org.scalameta.roles.CanEnroll[$T, $R, $E] {
            def e = ${Ident(mname)}
          }
          implicit def CanEnroll: _root_.org.scalameta.roles.CanEnroll[$T, $R, $E] = InternalCanEnroll
        """
        val mstats1 = mstats ++ List(defIsDefined, defSet) ++ instanceCanEnroll
        val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents2 { $self => ..$stats }"
        val mdef1 = ModuleDef(mods, mname, Template(mparents1, mself, mstats1))
        List(cdef1, mdef1)
      }

      def transformRoleModule(mdef: ModuleDef): ModuleDef = {
        val ModuleDef(mods, name, templ @ Template(parents, self, mstats)) = mdef
        val (parents1, isReadonly, tpe) = extractParents(mdef)
        val parents2 = parents1.filter(!_.toString.contains("Role["))
        val parents3 = parents2 :+ tq"_root_.scala.Product"
        val T = tpe
        val R = tq"_root_.org.scalameta.roles.internal.Dummy.type"
        val E = tq"${Ident(name)}.type"
        val parents4 = parents3 :+ tq"_root_.org.scalameta.roles.Enroller[$T, $R]"
        val defIsDefined = q"def isDefined(x: $T): Boolean = this.test(x)"
        val defGet = q"def get(x: $T): Option[$R] = if (this.test(x)) Some(_root_.org.scalameta.roles.internal.Dummy) else None"
        val defSet = {
          val body = {
            if (isReadonly) q"{ _root_.org.scalameta.invariants.`package`.require(this.test(x)); x }"
            else q"this.mark(x)"
          }
          q"def set[U <: $T](x: U, r: $R): U = $body"
        }
        val q"..$instanceCanEnroll" = q"""
          private object InternalCanEnroll extends _root_.org.scalameta.roles.CanEnroll[$T, $R, $E] {
            def e = ${Ident(name)}
          }
          implicit def CanEnroll: _root_.org.scalameta.roles.CanEnroll[$T, $R, $E] = InternalCanEnroll
        """
        val stats1 = mstats ++ List(defIsDefined, defGet, defSet) ++ instanceCanEnroll
        ModuleDef(casify(mods), name, Template(parents4, self, stats1))
      }

      val expanded = annottees match {
        case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transformRoleClass(cdef, mdef) ++ rest
        case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transformRoleClass(cdef, q"object ${name.toTermName}") ++ rest
        case (mdef @ ModuleDef(_, _, _)) :: rest => transformRoleModule(mdef) +: rest
        case annottee :: rest => c.abort(annottee.pos, "only classes and objects can be @role")
      }
      // println(expanded)
      q"{ ..$expanded; () }"
    }
  }
}
