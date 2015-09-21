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
  def transformSugarImplWithTRtype[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    // TODO: This is put here in order to make sure that `(tree: X).transform { ... }` actually returns X, not Tree.
    // It's obviously a hack and should be implemented elsewhere, but I didn't find the correct place for that.
    def unwrap(tpe: Type): Type = {
      if (tpe.typeSymbol == symbolOf[scala.collection.immutable.Seq[_]]) unwrap(tpe.typeArgs.head)
      else if (tpe.typeSymbol == symbolOf[scala.Option[_]]) unwrap(tpe.typeArgs.head)
      else tpe
    }
    val T = c.weakTypeOf[T]
    val V = unwrap(c.prefix.actualType.typeArgs.head)
    q"""
      implicit val unitRes: _root_.scala.meta.tql.TransformResultTr[Unit, $T] = {
        new _root_.scala.meta.tql.TransformResultTr[Unit, $T] {
          def get(t: $T, x: _root_.scala.meta.tql.MatchResult[Unit]): $T  = x.tree.getOrElse(t)
        }
      }
      implicit def withRes[A: _root_.org.scalameta.algebra.Monoid](implicit ev: _root_.org.scalameta.typelevel.=!=[A, Unit]): _root_.scala.meta.tql.TransformResultTr[A, ($V, A)] = {
        new _root_.scala.meta.tql.TransformResultTr[A, ($V, A)] {
          def get(t: $T, x: _root_.scala.meta.tql.MatchResult[A]): ($V, A)  = (x.tree.getOrElse(t).asInstanceOf[$V], x.result)
        }
      }
      ${c.prefix}.transforms(${transformSugarImpl[T](f)})
    """
  }
}
