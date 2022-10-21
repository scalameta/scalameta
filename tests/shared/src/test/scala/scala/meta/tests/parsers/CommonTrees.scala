package scala.meta.tests
package parsers

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
    def apply(): Self = Self(Name.Anonymous(), None)
    def unapply(tree: Tree): Boolean = tree match {
      case Self(Name.Anonymous(), None) => true
      case _ => false
    }
  }

  object EmptyCtor {
    def apply() = Ctor.Primary(Nil, Name.Anonymous(), Nil)
    def unapply(tree: Tree): Boolean = tree match {
      case Ctor.Primary(Nil, Name.Anonymous(), Nil) => true
      case _ => false
    }
  }

  object EmptyTemplate {
    def apply(): Template = Template(Nil, Nil, EmptySelf(), Nil)
    def unapply(tree: Tree): Boolean = tree match {
      case Template(Nil, Nil, EmptySelf(), Nil) => true
      case _ => false
    }
  }
}
