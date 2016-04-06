package scala.meta
package internal
package equality

private[meta] object NonRef {
  def unapply(tree: Tree): Option[Tree] = {
    if (tree.isInstanceOf[Ref]) None else Some(tree)
  }
}

private[meta] object NameRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case name: Term.Name => Some((name, Term.Name.privateTag))
      case name: Type.Name => Some((name, Type.Name.privateTag))
      case name: Ctor.Name => Some((name, Ctor.Name.privateTag))
      case Term.Select(NameRef(_, _), name) => Some((name, Term.Name.privateTag))
      case Type.Select(NameRef(_, _), name) => Some((name, Type.Name.privateTag))
      case Type.Project(NameRef(_, _), name) => Some((name, Type.Name.privateTag))
      case Ctor.Ref.Select(NameRef(_, _), name) => Some((name, Ctor.Name.privateTag))
      case _ => None
    }
  }
}

private[meta] object OpaqueRef {
  def unapply(tree: Tree): Option[(Name, Int)] = {
    tree match {
      case tree: Name.Indeterminate => Some((tree, Name.Indeterminate.privateTag))
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
