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
    def unapply(tree: Tree): Boolean = tree match {
      case Term.Param(Nil, Name.Anonymous(), None, None) => true
      case _ => false
    }
  }

  object EmptyCtor {
    def unapply(tree: Tree): Boolean = tree match {
      case Ctor.Primary(Nil, Ctor.Name("this"), Nil) => true
      case _ => false
    }
  }

  object EmptyTemplate {
    def unapply(tree: Tree): Boolean = tree match {
      case Template(Nil, Nil, EmptySelf(), None) => true
      case _ => false
    }
  }
}