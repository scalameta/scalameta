import scala.meta.internal.ast._

trait CommonTrees {
  object Nothing {
    def unapply(tree: Tree): Boolean = tree match {
      case Type.Name("Nothing") => true
      case _ => false
    }
  }

  object Any {
    def unapply(tree: Tree): Boolean = tree match {
      case Type.Name("Any") => true
      case _ => false
    }
  }

  object EmptySelf {
    def unapply(tree: Tree): Boolean = tree match {
      case Term.Param(Nil, None, None, None) => true
      case _ => false
    }
  }

  object EmptyTemplate {
    def unapply(tree: Tree): Boolean = tree match {
      case Template(Nil, Nil, Term.Param(Nil, None, None, None), Nil) => true
      case _ => false
    }
  }
}