package org.scalameta.reflection

import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

trait Attributed {
  self: GlobalToolkit =>

  import global._
  import definitions._
  import scala.reflect.internal.Flags._

  implicit class RichAttributedTree(tree: Tree) {
    def requireAttributed(): Unit = {
      var erroneous = 0
      var unattributed = 0
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
          else {
            if (tree.isErroneous) erroneous += 1
            else unattributed += 1
            offenders += ((tree, path))
          }
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
          // these trees are used as ensugared representations of named arguments
          // as such, they can't and don't have any types or symbols attributed to them
          case AssignOrNamedArg(lhs, rhs) => check(tree, skipSymbol = true, skipType = true)
          // this tree is used as part of an encoding for varargifying sequences
          // in this encoding _* doesn't have a symbol, only a type
          case Ident(tpnme.WILDCARD_STAR) => check(tree, skipSymbol = true)
          // `Function(Nil, EmptyTree)` is the secret parser marker which means trailing underscore
          // that's not even a valid type, so it can have neither type, nor symbol => we just skip it here
          case Typed(expr, Function(Nil, EmptyTree)) => check(expr)
          // https://groups.google.com/forum/#!topic/scala-internals/g81XE65jHc8
          // type ascriptions in arguments of classfile annotations aren't typechecked at all
          // our ensugarer represents this weird corner case with Typed(_, EmptyTree)
          case Typed(expr, EmptyTree) => check(expr)
          // underlying fields for early vals have broken originals of their tpts
          // therefore here we opt out of checking those
          // they don't matter anyway, because we're going to use tpts of fields' local counterparts
          case ValDef(_, _, _, rhs) if tree.symbol.hasFlag(PRESUPER) && tree.symbol.owner.isClass => check(rhs)
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
        val offenderDiagnostics = (erroneous, unattributed) match {
          case (0, 0) => unreachable
          case (0, _) => "unattributed"
          case (_, 0) => "erroneous"
          case (_, _) => "either unattributed or erroneous"
        }
        sys.error(s"""
          |Input scala.reflect tree is not fully attributed and can't be converted to a scala.meta tree.
          |The problem is caused by $offenderSummary that ${if (offenders.length == 1) "is" else "are"} $offenderDiagnostics:
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