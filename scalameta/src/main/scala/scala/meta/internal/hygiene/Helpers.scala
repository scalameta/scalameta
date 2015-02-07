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

object NamelikeRef {
  def unapply(tree: Tree): Option[Name] = {
    tree match {
      case name: Term.Name => Some(name)
      case name: Type.Name => Some(name)
      case name: Ctor.Name => Some(name)
      case Term.Select(NamelikeRef(_), name) => Some(name)
      case Type.Select(NamelikeRef(_), name) => Some(name)
      case Ctor.Ref.Select(NamelikeRef(_), name) => Some(name)
      case _ => None
    }
  }
}

object ThisRef {
  def unapply(tree: Tree): Option[Term.This] = {
    tree match {
      case tree: Term.This => Some(tree)
      case _ => None
    }
  }
}

object SuperRef {
  def unapply(tree: Tree): Option[Term.Super] = {
    tree match {
      case tree: Term.Super => Some(tree)
      case _ => None
    }
  }
}

object PrivateThisRef {
  def unapply(tree: Tree): Option[Mod.PrivateThis] = {
    tree match {
      case tree: Mod.PrivateThis => Some(tree)
      case _ => None
    }
  }
}

object PrivateWithinRef {
  def unapply(tree: Tree): Option[Mod.PrivateWithin] = {
    tree match {
      case tree: Mod.PrivateWithin => Some(tree)
      case _ => None
    }
  }
}

object ProtectedThisRef {
  def unapply(tree: Tree): Option[Mod.ProtectedThis] = {
    tree match {
      case tree: Mod.ProtectedThis => Some(tree)
      case _ => None
    }
  }
}

object ProtectedWithinRef {
  def unapply(tree: Tree): Option[Mod.ProtectedWithin] = {
    tree match {
      case tree: Mod.ProtectedWithin => Some(tree)
      case _ => None
    }
  }
}

object EquatableRef {
  def unapply(tree: Tree): Option[Tree] = {
    tree match {
      case NamelikeRef(_) => None
      case ThisRef(_) => None
      case SuperRef(_) => None
      case PrivateWithinRef(_) => None
      case ProtectedWithinRef(_) => None
      case _: Ref => Some(tree)
      case _ => None
    }
  }
}
