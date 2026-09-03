package scala.meta
package internal
package transversers

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class traverser extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TraverserMacros.impl
}

class TraverserMacros(val c: Context) extends TransverserMacros {
  import c.universe._

  def leafHandler(l: Leaf, treeName: TermName): Tree = {
    val relevantFields = l.fields
      .filter(f => !(f.tpe =:= typeOf[Any]) && !PrimitiveTpe.unapply(f.tpe))
    val recursiveTraversals = relevantFields.map(f => q"this.apply($treeName.${f.name})")
    q"..$recursiveTraversals"
  }

  def leafHandlerType(): Tree = UnitClass

  def generatedMethods(): Tree =
    q"""
      def apply(treeopt: $OptionClass[$TreeClass]): $UnitClass = treeopt match {
        case $SomeModule(tree) => apply(tree)
        case $NoneModule => // do nothing
      }

      def apply(trees: $ListClass[$TreeClass]): $UnitClass = {
        trees.foreach(apply(_))
      }

      def apply(trees: $SeqClass[$TreeClass]): $UnitClass = {
        trees.foreach(apply(_))
      }
    """
}
