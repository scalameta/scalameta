package scala.meta
package internal
package hygiene

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import impl._

// NOTE: Hygienic comparison operates exactly like structural comparison
// with a single exception of refs being treated differently, namely:
// 1) some structurally unequal refs (even having different types!) may compare equal when they refer to same defns
// 2) some structurally equal refs may compare unequal when they refer to different defns

// Now let's go through all of our refs and see how we should compare them.
// At the moment, we have 16 different AST nodes that are subtype of Ref:
// Name.Indeterminate,
// Term.Name, Term.Select,
// Type.Name, Type.Select, Type.Project, Type.Singleton,
// Pat.Type.Project,
// Ctor.Ref.Name, Ctor.Ref.Select, Ctor.Ref.Project, Ctor.Ref.Function,
// Selector.Wildcard, Selector.Name, Selector.Rename, Selector.Unimport.

// In the implementation that follows we do the following to compare these refs:
// 1) XXX.Name vs name-like XXX.Select/Type.Project, where XXX can be Term, Type or Ctor.Ref, are compared equal if they refer to the same defn
// 2) Term.This, Term.Super, as well as all PrivateXXX/ProtectedXXX are compared equal to themselves if they refer to the same defn
// 3) YYY.ZZZ vs YYY.ZZZ for the rest of the refs are compared structurally

// TODO: we should also use our compiler plugin powers to warn on Ref/Def comparisons
// because those indicate mistakes like `case q"foo.$bar" if bar == t"Foo".defs("bar") => ...`
// TODO: we should really generate bodies of those `equals` and `hashcode` with macros
// and check whether that would be faster that doing productPrefix/productElements

private[meta] object equals {
  private def refersToSameDefn(name1: Name, name2: Name): Boolean = {
    refersToSameDefn(name1.sigma.resolve(name1), name2.sigma.resolve(name2))
  }

  private def refersToSameDefn(denot1: Denotation, denot2: Denotation): Boolean = {
    denot1 != Denotation.Zero && denot2 != Denotation.Zero && denot1 == denot2
  }

  private def structuralEquals(tree1: Tree, tree2: Tree): Boolean = {
    // NOTE: for an exhaustive list of tree field types see
    // see /foundation/src/main/scala/org/scalameta/ast/internal.scala
    def loop(x1: Any, x2: Any): Boolean = (x1, x2) match {
      case (x1: Tree, x2: Tree) => semanticEquals(x1, x2)
      case (Some(x1), Some(x2)) => loop(x1, x2)
      case (Seq(xs1 @ _*), Seq(xs2 @ _*)) => xs1.zip(xs2).forall{ case (x1, x2) => loop(x1, x2) }
      case (x1, x2) => x1 == x2
    }
    def tagsEqual = tree1.internalTag == tree2.internalTag
    def fieldsEqual = tree1.productIterator.toList.zip(tree2.productIterator.toList).forall{ case (x1, x2) => loop(x1, x2) }
    tagsEqual && fieldsEqual
  }

  private def semanticEquals(tree1: Tree, tree2: Tree): Boolean = {
    (tree1, tree2) match {
      case (NonRef(tree1), NonRef(tree2)) => structuralEquals(tree1, tree2)
      case (NameRef(name1, tag1), NameRef(name2, tag2)) => tag1 == tag2 && refersToSameDefn(name1, name2)
      case (OpaqueRef(name1, tag1), OpaqueRef(name2, tag2)) => tag1 == tag2 && refersToSameDefn(name1, name2)
      case (StructuralRef(tree1), StructuralRef(tree2)) => structuralEquals(tree1, tree2)
      case _ => false
    }
  }

  def apply(tree1: api.Tree, tree2: api.Tree): Boolean = {
    if (tree1 == null || tree2 == null) tree1 == null && tree2 == null
    else semanticEquals(tree1.require[impl.Tree], tree2.require[impl.Tree])
  }
}

// TODO: no idea how to generate good hashcodes, but for now it doesn't matter much, I guess
// therefore I just took a random advice from StackOverflow: http://stackoverflow.com/questions/113511/hash-code-implementation
private[meta] object hashcode {
  private def structuralHashcode(tree: Tree): Int = {
    // NOTE: for an exhaustive list of tree field types see
    // see /foundation/src/main/scala/org/scalameta/ast/internal.scala
    def loop(x: Any): Int = x match {
      case null => 0
      case x: Tree => semanticHashcode(x)
      case Some(x) => loop(x)
      case Seq(xs @ _*) => xs.foldLeft(0)((acc, curr) => acc * 37 + loop(curr))
      case x => x.hashCode
    }
    val tagPart = tree.internalTag
    val fieldPart = loop(tree.productIterator.toList)
    tagPart * 37 + fieldPart
  }

  private def semanticHashcode(tree: Tree): Int = {
    tree match {
      case NameRef(name, tag) => name.sigma.resolve(name).hashCode() * 37 + tag
      case OpaqueRef(name, tag) => name.sigma.resolve(name).hashCode() * 37 + tag
      case _ => structuralHashcode(tree)
    }
  }

  def apply(tree: api.Tree): Int = {
    if (tree == null) return 0
    val semanticPart = semanticHashcode(tree.require[impl.Tree])
    val flavorPart = tree match { case _: Ctor.Ref => 3; case _: Term.Ref => 1; case _: Type.Ref => 2; case _ => 0 }
    semanticPart * 37 + flavorPart
  }
}
