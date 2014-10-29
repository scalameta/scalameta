import scala.meta._

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
      case Param.Term.Simple(Nil, None, None, None) => true
      case _ => false
    }
  }

  object EmptyTemplate {
    def unapply(tree: Tree): Boolean = tree match {
      case Aux.Template(Nil, Nil, Param.Term.Simple(Nil, None, None, None), Nil) => true
      case _ => false
    }
  }
}