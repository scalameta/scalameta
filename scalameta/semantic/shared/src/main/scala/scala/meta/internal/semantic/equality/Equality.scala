package scala.meta
package internal
package semantic

import scala.meta.classifiers._
import scala.meta.semantic._

// NOTE: Semantic comparison operates almost like structural comparison,
// but also taking into account envs, denots and typings.

// The difference with structural comparison is refs being treated differently, namely:
// 1) some structurally unequal refs (even having different types!) may compare equal when they refer to same defns
// 2) some structurally equal refs may compare unequal when they refer to different defns

// Now let's go through all of our refs and see how we should compare them.
// At the moment, we have 17 different AST nodes that are subtype of Ref:
// Name.Indeterminate,
// Term.Name, Term.Select, Term.ApplyUnary,
// Type.Name, Type.Select, Type.Project, Type.Singleton,
// Pat.Type.Project,
// Ctor.Ref.Name, Ctor.Ref.Select, Ctor.Ref.Project, Ctor.Ref.Function,
// Selector.Wildcard, Selector.Name, Selector.Rename, Selector.Unimport.

// In the implementation that follows we do the following to compare these refs:
// 1) XXX.Name vs name-like XXX.Select/Type.Project, where XXX can be Term, Type or Ctor.Ref, are compared equal if they refer to the same defn
// 2) Term.This, Term.Super, as well as all PrivateXXX/ProtectedXXX are compared equal to themselves if they refer to the same defn
// 3) YYY.ZZZ vs YYY.ZZZ for the rest of the refs are compared structurally

// TODO: I really don't like what I'm doing here.
// It would seem that instead of this bad-looking Any-based design,
// we should have Equality[T] { def equals; def hashCode }, which would be
// both modular (easily switch parts of the implementation) and type-safe.
// However, with the amount of AST nodes that we have,
// spelling all cases out manually will take prohibitively too much time.
// I would like to fix this in the future.

package object equality {
  def equals(x1: Any, x2: Any)(implicit m: Mirror): Boolean = customEquals(x1, x2)

  private def customEquals(x: Any, y: Any)(implicit m: Mirror): Boolean = (x, y) match {
    case (x, y) if x == null || y == null =>
      x == null && y == null
    case (x: Some[_], y: Some[_]) =>
      customEquals(x.get, y.get)
    case (x: None.type, y: None.type) =>
      true
    case (xs: Seq[_], ys: Seq[_]) =>
      xs.length == ys.length && xs.zip(ys).forall{ case (x, y) => customEquals(x, y) }
    case (x: Tree, y: Tree) =>
      def compareStructure(x: Tree, y: Tree) = {
        x.productPrefix == y.productPrefix &&
        customEquals(x.productIterator.toList, y.productIterator.toList)
      }
      def compareSemantics(x: Name, y: Name) = {
        x.symbol == y.symbol
      }
      (x, y) match {
        case (NonRef(x), NonRef(y)) => compareStructure(x, y)
        case (NameRef(namex, tagx), NameRef(namey, tagy)) => tagx == tagy && compareSemantics(namex, namey)
        case (OpaqueRef(namex, tagx), OpaqueRef(namey, tagy)) => tagx == tagy && compareSemantics(namex, namey)
        case (StructuralRef(x), StructuralRef(y)) => compareStructure(x, y)
        case _ => false
      }
    case _ =>
      x == y
  }

  def hashCode(x: Any)(implicit m: Mirror): Int = customHashcode(x)

  private def customHashcode(x: Any)(implicit m: Mirror): Int = x match {
    case null =>
      0
    case x: Option[_] =>
      x.map(customHashcode).getOrElse(0)
    case xs: Seq[_] =>
      xs.foldLeft(0)((acc, curr) => acc * 37 + customHashcode(curr))
    case x: Tree =>
      def hashStructure(x: Tree) = customHashcode(x.productPrefix) * 37 + customHashcode(x.productIterator.toList)
      def hashSemantics(x: Name) = customHashcode(x.symbol)
      x match {
        case NameRef(namex, tagx) => hashSemantics(namex) * 37 + tagx
        case OpaqueRef(namex, tagx) => hashSemantics(namex) * 37 + tagx
        case _ => hashStructure(x)
      }
    case _ =>
      x.hashCode
  }
}

package equality {
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
        case Term.Select(NameRef(_, _), name) => Some((name, 1))
        case Type.Select(NameRef(_, _), name) => Some((name, 2))
        case Type.Project(NameRef(_, _), name) => Some((name, 2))
        case Ctor.Ref.Select(NameRef(_, _), name) => Some((name, 3))
        case _ => None
      }
    }
  }

  object OpaqueRef {
    def unapply(tree: Tree): Option[(Name, Int)] = {
      tree match {
        case tree: Name.Indeterminate => Some((tree, 4))
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
}