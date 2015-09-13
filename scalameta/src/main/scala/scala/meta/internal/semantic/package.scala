package scala.meta
package internal

import scala.language.implicitConversions
import org.scalameta.default._
import org.scalameta.unreachable
import org.scalameta.invariants._
import scala.collection.mutable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => api}
import scala.meta.internal.ast._
import scala.meta.internal.ui.Attributes

package object semantic {
  implicit class XtensionHygieneDebug(debug: org.scalameta.debug.Debug.type) {
    def hygiene = sys.props("hygiene.debug") != null
  }

  implicit class XtensionAttributedTree[T <: api.Tree](tree: T) {
    def requireAttributed(): Unit = {
      val offenders = mutable.ListBuffer[(Tree, List[String])]()
      def traverse(tree: Tree, path: List[String]): Unit = {
        def check(tree: Tree): Boolean = {
          def checkEnv(tree: Tree): Boolean = tree.internalEnv.map(_ match {
            case Environment.Zero => true
            case _ => false
          }).getOrElse(true)

          def checkDenot(tree: Tree): Boolean = tree.internalDenot.map(_ match {
            case Denotation.Single(Prefix.Zero, _) =>
              true
            case Denotation.Single(Prefix.Type(prefix: Tree), _) =>
              traverse(prefix, path :+ "Denotation")
              true
            case Denotation.Multi(Prefix.Zero, _) =>
              true
            case Denotation.Multi(Prefix.Type(prefix: Tree), _) =>
              traverse(prefix, path :+ "Denotation")
              true
            case _ =>
              false
          }).getOrElse(true)

          def checkTyping(tree: Tree): Boolean = tree.internalTyping.map(_ match {
            case Typing.Recursive => true
            case Typing.Nonrecursive(_) => true
            case _ => false
          }).getOrElse(true)

          def checkExpansion(tree: Tree): Boolean = tree.internalExpansion.map(_ match {
            case Expansion.Identity => true
            case Expansion.Desugaring(_) => true
            case _ => false
          }).getOrElse(true)

          checkEnv(tree) && checkDenot(tree) && checkTyping(tree) && checkExpansion(tree)
        }
        def loop(x: Any): Unit = x match {
          case x: Tree => traverse(x, path :+ tree.productPrefix)
          case x: Seq[_] => x.foreach(loop)
          case x: Some[_] => loop(x.get)
          case x => // do nothing
        }
        if (!check(tree)) offenders += ((tree, path))
        tree.productIterator.toList.map(loop)
      }
      traverse(tree.require[Tree], Nil)
      if (offenders.nonEmpty) {
        val grouped = offenders.groupBy(_._1.productPrefix).toList.sortBy(_._1)
        def commaCommaAnd[T](list: List[T]): String = list.init.mkString(", ") + (if (list.length == 1) "" else " and ") + list.last
        // The problem is caused by 6 Idents that are either unattributed or erroneous:
        val offenderSummary = commaCommaAnd(grouped.map{ case (k, v) => s"${v.length} $k${if (v.length == 1) "" else "s"}" })
        // 1: _ (... CaseDef > Bind > UnApply > Typed > Ident)
        // 2: _ (...  DefDef > Match > CaseDef > Bind > Ident)
        // ...
        val offenderPrintout = grouped.flatMap(_._2).map({ case (tree, path) =>
          var prefix = tree.toString.replace("\n", " ")
          if (prefix.length > 60) prefix = prefix.take(60) + "..."
          val s_path = path.mkString(" > ")
          var suffix = if (s_path.length <= 40) s_path else "... " + s_path.takeRight(40)
          if (suffix != "") suffix = s" ($suffix)"
          s"$prefix$suffix"
        }).zipWithIndex.map{ case (s, i) => s"${i + 1}: $s" }.mkString("\n")
        sys.error(s"""
          |scala.meta tree is not fully attributed,
          |because $offenderSummary ${if (offenders.length == 1) "doesn't" else "don't"} have denotations:
          |$offenderPrintout
          |The tree that has caused problems is printed out below in its entirety:
          |$tree
          |${tree.show[Attributes]}
        """.stripMargin)
      }
    }

    def inheritAttrs(other: Tree): T = {
      def areCompatible = (
        !(tree.internalDenot.isEmpty ^ other.internalDenot.isEmpty) &&
        !(tree.internalTyping.isEmpty ^ other.internalTyping.isEmpty)
      )
      if (!areCompatible) sys.error(s"${tree.productPrefix} can't inherit attrs from ${other.productPrefix}")
      val result = tree match {
        case tree: Term.Name => tree.withAttrs(other.internalDenot.get, other.internalTyping.get)
        case tree: Ctor.Name => tree.withAttrs(other.internalDenot.get, other.internalTyping.get)
        case tree: Name => tree.withAttrs(other.internalDenot.get)
        case tree: Term => tree.withAttrs(other.internalTyping.get)
        case tree: Term.Param => tree.withAttrs(other.internalTyping.get)
        case _ => tree // do nothing
      }
      result.asInstanceOf[T]
    }
  }

  trait TypingLike { def typing: Typing }
  object TypingLike {
    implicit def typeIsTypingLike(tpe: => api.Type.Arg): TypingLike = new TypingLike { def typing = Typing.Nonrecursive(tpe) }
    implicit def typingIsTypingLike(typing0: Typing): TypingLike = new TypingLike { def typing = typing0 }
  }

  trait ExpansionLike { def expansion: Expansion }
  object ExpansionLike {
    implicit def termIsExpansionLike(term: api.Term): ExpansionLike = new ExpansionLike { def expansion = Expansion.Desugaring(term) }
    implicit def expansionIsExpansionLike(expansion0: Expansion): ExpansionLike = new ExpansionLike { def expansion = expansion0 }
  }
}