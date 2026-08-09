package scala.meta
package internal

import scala.meta.classifiers._
import scala.meta.internal.tokens.Chars

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag

package object trees {

  implicit class XtensionTreesName(private val name: Name) extends AnyVal {
    // some heuristic is needed to govern associativity and precedence of unquoted operators
    def isLeftAssoc: Boolean = name.is[Name.Quasi] || name.value.isLeftAssoc

    // opPrecedence?
    def precedence: Int = if (name.is[Name.Quasi]) 1 else name.value.precedence

    def isUnaryOp: Boolean = !name.is[Name.Quasi] && name.value.isUnaryOp

    def isAssignmentOp: Boolean = !name.is[Name.Quasi] && name.value.isAssignmentOp
  }

  implicit class XtensionTreesString(private val value: String) extends AnyVal {

    // some heuristic is needed to govern associativity and precedence of unquoted operators
    def isLeftAssoc: Boolean = value.last != ':'

    def isUnaryOp: Boolean = Unary.opMap.contains(value)

    def isAssignmentOp = precedence == 0

    def precedence: Int = {
      @inline
      def assignOr(p: Int) = if (value.last == '=') 0 else p
      @inline
      def assignIfPredOr(p: Int, pred: => Boolean) = if (value.last == '=' && pred) 0 else p
      (value.head: @switch) match {
        case '$' | '_' => assignOr(1) // also see more under default
        case '|' => assignOr(2)
        case '^' => assignOr(3)
        case '&' => assignOr(4)
        case '=' => 5 // never assign
        case '!' => assignIfPredOr(5, value.length > 2) // excludes !=
        case '<' | '>' => assignIfPredOr(6, value.length > 2) // excludes <=, >=
        case ':' => assignOr(7)
        case '+' | '-' => assignOr(8)
        case '*' | '/' | '%' => assignOr(9)
        case ch =>
          if (Chars.isTypeMask(Chars.scalaLetterTypeMask)(ch)) 1
          else assignIfPredOr(10, Chars.isOperatorPart(ch))
      }
    }
  }

  implicit class XtensionTreesTermRef(private val tree: Term.Ref) extends AnyVal {
    @tailrec
    def isQualId: Boolean = tree match {
      case _: Term.Ref.Quasi => true
      case _: Term.Name => true
      case Term.Select(qual: Term.Ref, _) => qual.isQualId
      case _ => false
    }
    @tailrec
    def isPath: Boolean = tree match {
      case _: Term.Ref.Quasi | _: Term.This | _: Term.Name | _: Term.Anonymous => true
      case Term.Select(_: Term.Super | _: Term.Quasi, _) => true
      case Term.Select(qual: Term.Ref, _) => qual.isPath
      case _ => false
    }
  }

  implicit class XtensionTreesMods(private val mods: collection.Iterable[Mod]) extends AnyVal {
    def has[T <: Mod](implicit tag: ClassTag[T]): Boolean = mods.exists { case m =>
      tag.runtimeClass.isAssignableFrom(m.getClass)
    }
    def first[T <: Mod](implicit tag: ClassTag[T]): Option[T] = mods
      .collectFirst { case m if tag.runtimeClass.isAssignableFrom(m.getClass) => m.asInstanceOf[T] }
  }

  implicit class XtensionTreesStat(private val stat: Stat) extends AnyVal {
    def isTopLevelStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Export => true
      case _: Pkg => true
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _: Defn.Object => true
      case _: Defn.Def => true
      case _: Defn.Given => true
      case _: Defn.GivenAlias => true
      case _: Defn.Enum => true
      case _: Defn.ExtensionGroup => true
      case _: Defn.Val => true
      case _: Defn.Type => true
      case _: Decl.Type => true
      case _: Term.EndMarker => true
      case _: Defn.Var => true
      case _: Pkg.Object => true
      case _ => false
    }
    def isBlockStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Term => true
      case _: Defn => true
      case _: Decl.Type => true
      case _ => false
    }
  }

  implicit class XtensionTreesCase(private val tree: Case) extends AnyVal {
    def stats: List[Stat] = tree.body match {
      case Term.Block(stats) => stats
      case body => List(body)
    }
  }

  @tailrec
  def arrayClass(clazz: Class[_], rank: Int): Class[_] = {
    import scala.runtime.ScalaRunTime
    Predef.require(rank >= 0)
    if (rank == 0) clazz else arrayClass(ScalaRunTime.arrayClass(clazz), rank - 1)
  }

  @inline
  private[meta] def isQuasi(tree: Tree): Boolean = tree.isInstanceOf[Quasi]

  @inline
  private[meta] def isQuasiOr(tree: Tree, check: => Boolean): Boolean = isQuasi(tree) || check

}
