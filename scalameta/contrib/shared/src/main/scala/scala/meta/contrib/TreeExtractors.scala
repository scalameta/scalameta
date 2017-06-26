package scala.meta
package contrib

trait TreeExtractors {
  object Select {
    def unapply(tree: Tree): Option[(Term, Name)] = tree match {
      case Term.Select(qual, name) => Some(qual -> name)
      case Type.Select(qual, name) => Some(qual -> name)
      case _ => None
    }
  }
}
