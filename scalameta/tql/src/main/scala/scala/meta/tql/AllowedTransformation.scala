package scala.meta
package tql

import scala.meta.internal.tql._
import scala.language.experimental.macros

trait AllowedTransformation[I, O]

object AllowedTransformation {
  /**
   * Create an AllowedTransformation[I, O] If :
   *  Upper Branch* of O is a subtype of Upper Branch of I
   *  Where I and O are both subtypes of T.
   *  Example:
   *  materializerAllowedTransformation[Tree, Lit.Int, Lit] would work since:
   *  Lit.Int is a Leaf* => its upper branch is Lit and Lit is a subtype of Lit
   *
   *  But materialize[Tree, Lit.Int, Term.If] wouldn't work
   *
   *  Branch, Leaf  in the Adt scala.meta sense of the term
   */
  implicit def materialize[I <: Tree, O <: Tree]: AllowedTransformation[I, O] =
    macro AllowedTransformationMacros.materialize[Tree, I, O]
}
