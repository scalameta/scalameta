package scala.meta
package internal
package hygiene

import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import impl._

object NonRef {
  def unapply(tree: Tree): Option[Tree] = {
    if (tree.isInstanceOf[Ref]) None else Some(tree)
  }
}

object NameRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case name: Term.Name => Some((name, Term.Name.$tag))
      case name: Type.Name => Some((name, Type.Name.$tag))
      case name: Ctor.Name => Some((name, Ctor.Name.$tag))
      case Term.Select(NameRef(_, _), name) => Some((name, Term.Name.$tag))
      case Type.Select(NameRef(_, _), name) => Some((name, Type.Name.$tag))
      case Type.Project(NameRef(_, _), name) => Some((name, Type.Name.$tag))
      case Ctor.Ref.Select(NameRef(_, _), name) => Some((name, Ctor.Name.$tag))
      case _ => None
    }
  }
}

object OpaqueRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case tree: Term.This => Some((tree, Term.This.$tag))
      case tree: Term.Super => Some((tree, Term.Super.$tag))
      case tree: Mod.PrivateThis => Some((tree, Mod.PrivateThis.$tag))
      case tree: Mod.PrivateWithin => Some((tree, Mod.PrivateWithin.$tag))
      case tree: Mod.ProtectedThis => Some((tree, Mod.ProtectedThis.$tag))
      case tree: Mod.ProtectedWithin => Some((tree, Mod.ProtectedWithin.$tag))
      case _ => None
    }
  }
}

object StructuralRef {
  def unapply(tree: Tree): Option[Tree] = {
    tree match {
      case NameRef(_, _) => None
      case OpaqueRef(_, _) => None
      case _: Ref => Some(tree)
      case _ => None
    }
  }
}
