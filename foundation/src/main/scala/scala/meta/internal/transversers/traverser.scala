package scala.meta
package internal
package transversers

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class traverser extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TraverserMacros.impl
}

class TraverserMacros(val c: Context) extends TransverserMacros {
  import c.universe._

  def leafHandler(l: Leaf): Tree = {
    val relevantFields = l.fields.filter(f => !(f.tpe =:= typeOf[Any]) && !(f.tpe =:= typeOf[String]))
    val recursiveTraversals = relevantFields.map(f => {
      val fieldName = f.name
      val traverserName = TermName("traverse" + suffix(f))
      q"this.$traverserName($fieldName)"
    })
    q"..$recursiveTraversals"
  }

  def generatedMethods(cases: List[CaseDef]): Tree = {
    q"""
      def traverse(tree: $TreeClass): $UnitClass = {
        tree match { case ..$cases }
      }

      def traverseOpt(treeopt: $OptionClass[$TreeClass]): $UnitClass = treeopt match {
        case $SomeModule(tree) => traverse(tree)
        case $NoneModule => // do nothing
      }

      def traverseSeq(trees: $SeqClass[$TreeClass]): $UnitClass = {
        val it = trees.iterator
        while (it.hasNext) {
          traverse(it.next)
        }
      }

      def traverseOptSeq(treesopt: $OptionClass[$SeqClass[$TreeClass]]): $UnitClass = treesopt match {
        case $SomeModule(trees) => traverseSeq(trees)
        case $NoneModule => // do nothing
      }

      def traverseSeqSeq(treess: $SeqClass[$SeqClass[$TreeClass]]): $UnitClass = {
        val it = treess.iterator
        while (it.hasNext) {
          traverseSeq(it.next)
        }
      }
    """
  }
}
