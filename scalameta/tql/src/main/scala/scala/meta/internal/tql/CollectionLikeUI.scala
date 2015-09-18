package scala.meta
package internal
package tql

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.meta.tql._

/**
 * This class, much like CombinatorMacros, define several bundled macros used to enhance
 * or to make less boilerplaty the CollectionLikeUI
 * */
private[meta] class CollectionLikeUIMacros(override val c: Context) extends CombinatorMacros(c) {
  import c.universe._
  override val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  /**
   * See comments in parent class (CombinatorMacros).
   * This method has to be overriden because the 'prefix' has to be added in front of the
   * rewritten function.
   * */
  override def filterSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    //I know it's ugly but that's the only way I found to do it.
    val (lhs, _) =  getLUBsfromPFs[T](f)
    q"${c.prefix}.guard[$lhs]($f)"
  }

  /**
   * The implementation of transform is special in CollectionLikeUI because we have to make sure that several
   * transformations can be re-written.
   * */
  def transformSugarImplWithTRtype[T : c.WeakTypeTag](f: c.Tree): c.Tree =
    q"${c.prefix}.transforms(${transformSugarImpl[T](f)})"
}
