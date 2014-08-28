package scala.meta
package internal.hosts.scalacompiler
package typechecker

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import scala.reflect.internal.Mode

trait Analyzer extends NscAnalyzer {
  import global._
  import definitions._

  override def newTyper(context: Context) = new Typer(context) {
    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      super.typed1(tree, mode, pt)
    }
  }
}