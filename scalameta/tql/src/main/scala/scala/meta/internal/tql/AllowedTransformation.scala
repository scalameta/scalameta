package scala.meta
package internal
package tql

import scala.reflect.macros.blackbox.Context
import scala.meta.tql._
import scala.meta.internal.ast.{Reflection => AstReflection}

private[meta] class AllowedTransformationMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  def materialize[T : c.WeakTypeTag, I : c.WeakTypeTag, O : c.WeakTypeTag]: c.Expr[AllowedTransformation[I, O]] = {
    // TODO: Now when scala.meta.internal.ast has been merged into scala.meta,
    // the old logic of validating I => O transformations doesn't work.
    // Since I'm not sure whether TQL is going to make it into 2.10, I'm putting in a stub here for the time being.
    c.Expr(q"new _root_.scala.meta.tql.AllowedTransformation[${implicitly[c.WeakTypeTag[I]]}, ${implicitly[c.WeakTypeTag[O]]}] {}")
  }
}
