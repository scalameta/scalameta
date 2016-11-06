package scala.meta
package internal
package equality

import scala.meta.internal.semantic._

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

object Semantic {
  // The previous implementations were buggy and triggered infinite recursion.
  def equals(x1: Any, x2: Any): Boolean = x1 == x2
  def hashCode(x: Any): Int = x.##
}
