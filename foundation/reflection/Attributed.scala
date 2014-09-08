package org.scalameta.reflection

import scala.collection.mutable
import org.scalameta.invariants._

trait Attributed {
  self: GlobalToolkit =>

  import global._
  import definitions._

  implicit class RichAttributedTree(tree: Tree) {
    def ensureAttributed(): Unit = {
      val offenders = mutable.ListBuffer[(Tree, List[Tree])]()
      object traverser extends Traverser {
        private var path = List[Tree]()
        private def drilldown[T](tree: Tree)(op: Tree => T): T = try { path ::= tree; op(tree) } finally { path = path.tail }
        private def check(tree: Tree, skipSymbol: Boolean = false, skipType: Boolean = false): Unit = {
          val ok = {
            !tree.isErroneous &&
            (tree.canHaveAttrs ==> (skipType || (tree.tpe != null))) &&
            ((tree.canHaveAttrs && tree.hasSymbolField) ==> (skipSymbol || (tree.symbol != null && tree.symbol != NoSymbol)))
          }
          if (ok) super.traverse(tree)
          else { offenders += ((tree, path)) }
        }
        override def traverse(tree: Tree): Unit = drilldown(tree) {
          // imports and selectors of imports aren't attributed by the typechecker
          case Import(qual, selectors) => check(qual)
          // patmat wildcards used in binds and typeds don't have symbols
          case Ident(nme.WILDCARD) => check(tree, skipSymbol = true)
          // skip verification of Literal(Constant(0))
          // https://groups.google.com/forum/#!topic/scala-internals/gppfuY7-tdA
          case Literal(Constant(0)) =>
          // literals don't have symbols
          case Literal(const) => check(tree, skipSymbol = true)
          // template symbols aren't set when typechecking compound type trees, and they are dummies anyway, so it doesn't really matter
          // https://groups.google.com/forum/#!topic/scala-internals/6ebL7waMav8
          case Template(parents, self, stats) => check(tree, skipSymbol = true)
          case _ => check(tree)
        }
      }
      traverser.traverse(tree)
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
          val suffix = path.scanLeft("")((suffix, tree) => {
            if (suffix.length == 0) tree.productPrefix
            else tree.productPrefix + " > " + suffix
          }) match {
            case _ :+ last if last.length <= 40 => last
            case list => "... " + list.dropWhile(_.length <= 40).head.takeRight(40)
          }
          s"$prefix ($suffix)"
        }).zipWithIndex.map{ case (s, i) => s"${i + 1}: $s" }.mkString("\n")
        sys.error(s"""
          |Input Scala tree is not fully attributed and can't be converted to a Palladium tree.
          |The problem is caused by $offenderSummary that ${if (offenders.length == 1) "is" else "are"} either unattributed or erroneous:
          |$offenderPrintout
          |The input tree that has caused problems to the converter is printed out below:
          |(Note that showRaw output at the end of the printout is supposed to contain ids and types)
          |$tree
          |${showRaw(tree, printIds = true, printTypes = true)}
        """.stripMargin)
      }
    }
  }
}