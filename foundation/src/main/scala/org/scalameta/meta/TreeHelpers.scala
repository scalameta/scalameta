package org.scalameta.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import impl._
import org.scalameta.invariants._
import org.scalameta.unreachable

trait TreeHelpers {
  self: Toolkit =>

  implicit class RichNameStat(stat: Stat) {
    def binders: Seq[Name] = stat match {
      case tree: Decl.Val => tree.pats.map(_.name)
      case tree: Decl.Var => tree.pats.map(_.name)
      case tree: Decl.Def => List(tree.name)
      case tree: Decl.Type => List(tree.name)
      case tree: Defn.Val => ???
      case tree: Defn.Var => ???
      case tree: Defn.Def => List(tree.name)
      case tree: Defn.Macro => List(tree.name)
      case tree: Defn.Type => List(tree.name)
      case tree: Defn.Class => List(tree.name)
      case tree: Defn.Trait => List(tree.name)
      case tree: Defn.Object => List(tree.name)
      case       Pkg(name: impl.Term.Name, _) => List(name)
      case       Pkg(impl.Term.Select(_, name: impl.Term.Name), _) => List(name)
      case tree: Pkg.Object => List(tree.name)
      case tree: Ctor.Secondary => List(tree.name)
      case _ => Nil
    }
    def member: Member = stat match {
      case Decl.Val(_, List(member: Pat.Var.Term), _) => member
      case Decl.Var(_, List(member: Pat.Var.Term), _) => member
      case Defn.Val(_, List(member: Pat.Var.Term), _, _) => member
      case Defn.Var(_, List(member: Pat.Var.Term), _, _) => member
      case tree: Member => tree
      case _ => sys.error(s"unsupported stat ${stat.productPrefix}: $stat")
    }
  }

  implicit class RichStatMember(member: Member) {
    private def firstNonPatParent(pat: Pat): Option[Tree] = {
      pat.parent.collect{case pat: Pat => pat}.flatMap(firstNonPatParent).orElse(pat.parent.map(_.require[Tree]))
    }
    def stat: Stat = member match {
      case tree: Pat.Var.Term => firstNonPatParent(tree).get.require[Stat]
      case stat: Stat => stat
      case _ => sys.error(s"unsupported member ${member.productPrefix}: $member")
    }
  }
}
