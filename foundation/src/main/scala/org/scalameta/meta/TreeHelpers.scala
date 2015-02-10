package org.scalameta.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import impl._

trait TreeHelpers {
  self: Toolkit =>

  implicit class RichNameStat(stat: Stat) {
    def binders: Seq[Name] = stat match {
      case tree: impl.Term.Name if tree.isBinder => List(tree)
      case tree: impl.Decl.Val => tree.pats.map(_.name)
      case tree: impl.Decl.Var => tree.pats.map(_.name)
      case tree: impl.Decl.Def => List(tree.name)
      case tree: impl.Decl.Type => List(tree.name)
      case tree: impl.Defn.Val => ???
      case tree: impl.Defn.Var => ???
      case tree: impl.Defn.Def => List(tree.name)
      case tree: impl.Defn.Macro => List(tree.name)
      case tree: impl.Defn.Type => List(tree.name)
      case tree: impl.Defn.Class => List(tree.name)
      case tree: impl.Defn.Trait => List(tree.name)
      case tree: impl.Defn.Object => List(tree.name)
      case       impl.Pkg(name: impl.Term.Name, _) => List(name)
      case       impl.Pkg(impl.Term.Select(_, name: impl.Term.Name), _) => List(name)
      case tree: impl.Pkg.Object => List(tree.name)
      case tree: impl.Ctor.Secondary => List(tree.name)
      case _ => Nil
    }
  }

  implicit class RichStatMember(member: Member) {
    private def firstNonPatParent(pat: Pat): Option[Tree] = {
      pat.parent.collect{case pat: Pat => pat}.flatMap(firstNonPatParent).orElse(pat.parent.map(_.asInstanceOf[Tree]))
    }
    def stat: Stat = member match {
      case tree: Term.Name if tree.isBinder => firstNonPatParent(tree).get.asInstanceOf[Stat]
      case stat: Stat => stat
    }
  }
}
