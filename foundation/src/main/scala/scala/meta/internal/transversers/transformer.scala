package scala.meta
package internal
package transversers

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class transformer extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TransformerMacros.impl
}

class TransformerMacros(val c: Context) extends TransverserMacros {
  import c.universe._

  def leafHandler(l: Leaf): Tree = {
    q"???"
  }

  def generatedMethods(cases: List[CaseDef]): Tree = {
    q"""
      def transform(tree: $TreeClass): $TreeClass = {
        tree match { case ..$cases }
      }

      def transform(treeopt: $OptionClass[$TreeClass]): $OptionClass[$TreeClass] = treeopt match {
        case $SomeModule(tree) =>
          val tree1 = transform(tree)
          if (tree eq tree1) treeopt
          else $SomeModule(tree1)
        case $NoneModule =>
          $NoneModule
      }

      def transform(trees: $SeqClass[$TreeClass]): $SeqClass[$TreeClass] = {
        var same = true
        val buf = $ListBufferModule[$TreeClass]()
        val it = trees.iterator
        while (it.hasNext) {
          val tree = it.next
          val tree1 = transform(tree)
          if (tree ne tree1) same = false
          buf += tree1
        }
        if (same) trees
        else buf.toList
      }

      def transform(treesopt: $OptionClass[$SeqClass[$TreeClass]])(implicit hack: $Hack1Class): $OptionClass[$SeqClass[$TreeClass]] = treesopt match {
        case $SomeModule(trees) =>
          val trees1 = transform(trees)
          if (trees eq trees1) treesopt
          else $SomeModule(trees1)
        case $NoneModule =>
          $NoneModule
      }

      def transform(treess: $SeqClass[$SeqClass[$TreeClass]])(implicit hack: $Hack2Class): $SeqClass[$SeqClass[$TreeClass]] = {
        var same = true
        val buf = $ListBufferModule[$SeqClass[$TreeClass]]()
        val it = treess.iterator
        while (it.hasNext) {
          val trees = it.next
          val trees1 = transform(trees)
          if (trees ne trees1) same = false
          buf += trees1
        }
        if (same) treess
        else buf.toList
      }
    """
  }
}
