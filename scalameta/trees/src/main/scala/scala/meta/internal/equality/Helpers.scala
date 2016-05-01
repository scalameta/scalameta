package scala.meta
package internal
package equality

import scala.meta.classifiers._

object NonRef {
  def unapply(tree: Tree): Option[Tree] = {
    if (tree.is[Ref]) None else Some(tree)
  }
}

object NameRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case name: Term.Name => Some((name, 1))
      case name: Type.Name => Some((name, 2))
      case name: Ctor.Name => Some((name, 3))
      case Term.Select(NameRef(_, _), name) => Some((name, 4))
      case Type.Select(NameRef(_, _), name) => Some((name, 5))
      case Type.Project(NameRef(_, _), name) => Some((name, 6))
      case Ctor.Ref.Select(NameRef(_, _), name) => Some((name, 7))
      case _ => None
    }
  }
}

object OpaqueRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case tree: Name.Indeterminate => Some((tree, 8))
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
