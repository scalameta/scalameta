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

// TODO: I really don't like what I'm doing here.
// It would seem that instead of this bad-looking Any-based design,
// we should have Equality[T] { def equals; def hashCode }, which would be
// both modular (easily switch parts of the implementation) and type-safe.
// However, with the amount of AST nodes that we have,
// spelling all cases out manually will take prohibitively too much time.
// I would like to fix this in the future.

object Semantic {
  def equals(x1: Any, x2: Any): Boolean = customEquals(x1, x2)

  private def customEquals(x: Any, y: Any): Boolean = (x, y) match {
    case (x, y) if x == null || y == null =>
      x == null && y == null
    case (x: Some[_], y: Some[_]) =>
      customEquals(x.get, y.get)
    case (x: None.type, y: None.type) =>
      true
    case (xs: Seq[_], ys: Seq[_]) =>
      xs.length == ys.length && xs.zip(ys).forall{ case (x, y) => customEquals(x, y) }
    case (x: Environment, y: Environment) =>
      x == y
    case (x: Prefix, y: Prefix) =>
      (x, y) match {
        case (Prefix.Type(x), Prefix.Type(y)) => customEquals(x, y)
        case _ => x == y
      }
    case (x: Denotation, y: Denotation) =>
      customEquals(x.prefix, y.prefix) && customEquals(x.symbols, y.symbols)
    case (x: Typing, y: Typing) =>
      (x, y) match {
        case (Typing.Nonrecursive(x), Typing.Nonrecursive(y)) => customEquals(x, y)
        case _ => x == y
      }
    case (x: Tree, y: Tree) =>
      def syntaxPart = {
        def compareStructure(x: Tree, y: Tree) = {
          x.productPrefix == y.productPrefix &&
          customEquals(x.productIterator.toList, y.productIterator.toList)
        }
        def compareSemantics(x: Name, y: Name) = {
          x.denot != Denotation.None && y.denot != Denotation.None && x.denot == y.denot
        }
        (x, y) match {
          case (NonRef(x), NonRef(y)) => compareStructure(x, y)
          case (NameRef(namex, tagx), NameRef(namey, tagy)) => tagx == tagy && compareSemantics(namex, namey)
          case (OpaqueRef(namex, tagx), OpaqueRef(namey, tagy)) => tagx == tagy && compareSemantics(namex, namey)
          case (StructuralRef(x), StructuralRef(y)) => compareStructure(x, y)
          case _ => false
        }
      }
      def envPart = customEquals(x.privateEnv, y.privateEnv)
      def denotPart = customEquals(x.privateDenot, y.privateDenot)
      def typingPart = customEquals(x.privateTyping, y.privateTyping)
      syntaxPart && envPart && denotPart && typingPart
    case _ =>
      x == y
  }

  def hashCode(x: Any): Int = customHashcode(x)

  private def customHashcode(x: Any): Int = x match {
    case null =>
      0
    case x: Option[_] =>
      x.map(customHashcode).getOrElse(0)
    case xs: Seq[_] =>
      xs.foldLeft(0)((acc, curr) => acc * 37 + customHashcode(curr))
    case x: Environment =>
      x.hashCode
    case x: Prefix =>
      x match {
        case Prefix.None => 0
        case Prefix.Type(tpe) => customHashcode(tpe)
      }
    case x: Denotation =>
      x match {
        case Denotation.None => 0
        case Denotation.Single(prefix, symbol) => customHashcode(prefix) * 37 + customHashcode(symbol)
        case Denotation.Multi(prefix, symbols) => customHashcode(prefix) * 37 + customHashcode(symbols)
      }
    case x: Typing =>
      x match {
        case Typing.None => 0
        case Typing.Recursive => 1
        case Typing.Nonrecursive(tpe) => customHashcode(tpe)
      }
    case x: Tree =>
      def syntaxPart = {
        def hashStructure(x: Tree) = customHashcode(x.productPrefix) * 37 + customHashcode(x.productIterator.toList)
        def hashSemantics(x: Name) = customHashcode(x.denot)
        x match {
          case NameRef(namex, tagx) => hashSemantics(namex) * 37 + tagx
          case OpaqueRef(namex, tagx) => hashSemantics(namex) * 37 + tagx
          case _ => hashStructure(x)
        }
      }
      def envPart = customHashcode(x.privateEnv)
      def denotPart = customHashcode(x.privateDenot)
      def typingPart = customHashcode(x.privateTyping)
      customHashcode(List(syntaxPart, envPart, denotPart, typingPart))
    case _ =>
      x.hashCode
  }
}
