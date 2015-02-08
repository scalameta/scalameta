package org.scalameta.meta

import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import scala.meta.{Exception => SemanticException, _}
import scala.collection.mutable
import scala.meta.internal.hygiene._

trait Attributed {
  self: Toolkit =>

  // TODO: so wow, much copy/paste (wrt reflection/Attributed.scala)
  implicit class RichAttributedMetaTree(tree: Tree) {
    def requireAttributed(): Unit = {
      val offenders = mutable.ListBuffer[(Tree, List[String])]()
      def traverse(tree: Tree, path: List[String]): Unit = {
        def check(tree: Tree): Boolean = tree match {
          case tree: impl.Name =>
            (tree.denot, tree.sigma) match {
              case (Denotation.Precomputed(Prefix.Zero, _), Sigma.Naive) =>
                true
              case (Denotation.Precomputed(Prefix.Type(prefix), _), Sigma.Naive) =>
                traverse(prefix, path :+ "Denotation")
                true
              case _ =>
                false
            }
          case _ =>
            true
        }
        def loop(x: Any): Unit = x match {
          case x: Tree => traverse(x, path :+ tree.productPrefix)
          case x: List[_] => x.foreach(loop)
          case x: Some[_] => loop(x.get)
          case x => // do nothing
        }
        if (!check(tree)) offenders += ((tree, path))
        tree.productIterator.map(loop)
      }
      traverse(tree, Nil)
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
        throw new SemanticException(s"""
          |Input scala.meta tree is not fully attributed and can't be converted to a scala.reflect artifact.
          |The problem is caused by $offenderSummary that ${if (offenders.length == 1) "doesn't" else "don't"} have denotations:
          |$offenderPrintout
          |The input tree that has caused problems to the converter is printed out below:
          |(Note that show[Semantics] output at the end of the printout is supposed to contain denotations)
          |$tree
          |${tree.show[Semantics]}
        """.stripMargin)
      }
    }
  }
}
