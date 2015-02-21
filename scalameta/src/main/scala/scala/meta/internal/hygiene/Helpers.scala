package scala.meta
package internal
package hygiene

import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import impl._

private[meta] object NonRef {
  def unapply(tree: Tree): Option[Tree] = {
    if (tree.isInstanceOf[Ref]) None else Some(tree)
  }
}

private[meta] object NameRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case name: Term.Name => Some((name, Term.Name.internalTag))
      case name: Type.Name => Some((name, Type.Name.internalTag))
      case name: Ctor.Name => Some((name, Ctor.Name.internalTag))
      case Term.Select(NameRef(_, _), name) => Some((name, Term.Name.internalTag))
      case Type.Select(NameRef(_, _), name) => Some((name, Type.Name.internalTag))
      case Type.Project(NameRef(_, _), name) => Some((name, Type.Name.internalTag))
      case Ctor.Ref.Select(NameRef(_, _), name) => Some((name, Ctor.Name.internalTag))
      case _ => None
    }
  }
}

private[meta] object OpaqueRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case tree: Term.This => Some((tree, Term.This.internalTag))
      case tree: Term.Super => Some((tree, Term.Super.internalTag))
      case tree: Name.Indeterminate => Some((tree, Name.Indeterminate.internalTag))
      case tree: Name.Imported => Some((tree, Name.Imported.internalTag))
      case _ => None
    }
  }
}

private[meta] object StructuralRef {
  def unapply(tree: Tree): Option[Tree] = {
    tree match {
      case NameRef(_, _) => None
      case OpaqueRef(_, _) => None
      case _: Ref => Some(tree)
      case _ => None
    }
  }
}
