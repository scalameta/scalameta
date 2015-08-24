package scala.meta
package internal
package semantic

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import impl._
import scala.meta.semantic.{Context => SemanticContext}
import scala.reflect.macros.blackbox.{Context => BlackboxContext}

object Equivalence {
  private def normalize(tree: api.Tree)(implicit c: SemanticContext): api.Tree = {
    val ttree = c.typecheck(tree).require[impl.Tree]
    ttree match {
      case tterm: meta.Term => tterm.desugar
      case ttpe: meta.Type => ttpe.dealias
      case ttree => tree
    }
  }

  def equals(tree1: api.Tree, tree2: api.Tree)(implicit c: SemanticContext): Boolean = {
    Equality.equals(normalize(tree1), normalize(tree2))
  }

  def hashCode(tree: api.Tree)(implicit c: SemanticContext): Int = {
    Equality.hashCode(normalize(tree))
  }
}

class EquivalenceMacros(val c: BlackboxContext) {
  import c.universe._

  def allow[T1, T2](implicit T1: WeakTypeTag[T1], T2: WeakTypeTag[T2]): Tree = {
    val helper = new EqualityMacros(c)
    val result = helper.allow(T1.asInstanceOf[helper.c.WeakTypeTag[T1]], T2.asInstanceOf[helper.c.WeakTypeTag[T2]])
    result.asInstanceOf[Tree]
  }
}
