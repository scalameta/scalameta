package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import org.scalameta.ast.{Reflection => AstReflection}
import org.scalameta.invariants._
import org.scalameta.unreachable

private[meta] class SignatureMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  import u._
  def publish(tree: c.Tree): c.Tree = q"$tree: ${tree.tpe.publish}"
}