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
    val recursiveTraversals = relevantFields.map(f => q"this.traverse(${f.name})")
    q"..$recursiveTraversals"
  }

  def generatedMethods(cases: List[CaseDef]): Tree = {
    q"""
      def traverse(tree: $TreeClass): $UnitClass = {
        tree match { case ..$cases }
      }

      def traverse(treeopt: $OptionClass[$TreeClass]): $UnitClass = treeopt match {
        case $SomeModule(tree) => traverse(tree)
        case $NoneModule => // do nothing
      }

      def traverse(trees: $SeqClass[$TreeClass]): $UnitClass = {
        val it = trees.iterator
        while (it.hasNext) {
          traverse(it.next)
        }
      }

      def traverse(treesopt: $OptionClass[$SeqClass[$TreeClass]])(implicit hack: $Hack1Class): $UnitClass = treesopt match {
        case $SomeModule(trees) => traverse(trees)
        case $NoneModule => // do nothing
      }

      def traverse(treess: $SeqClass[$SeqClass[$TreeClass]])(implicit hack: $Hack2Class): $UnitClass = {
        val it = treess.iterator
        while (it.hasNext) {
          traverse(it.next)
        }
      }
    """
  }
}
