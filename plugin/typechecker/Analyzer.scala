package scala.meta
package internal.hosts.scalacompiler
package typechecker

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import org.scalameta.reflection.Metadata
import scala.reflect.internal.Mode

trait Analyzer extends NscAnalyzer with Metadata {
  import global._
  import definitions._

  override def newTyper(context: Context) = new Typer(context) {
    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      val result = super.typed1(tree, mode, pt)
      tree match {
        case tree: Ident => result.appendMetadata("originalIdent" -> tree)
        case _ => result
      }
    }
  }
}