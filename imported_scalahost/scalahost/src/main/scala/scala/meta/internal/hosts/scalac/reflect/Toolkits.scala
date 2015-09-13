package scala.meta.internal.hosts.scalac
package reflect

import scala.reflect.macros.{Universe => MacroUniverse}
import scala.tools.nsc.Global
import org.scalameta.ast.{Reflection => AstReflection}

trait MacroToolkit extends Metadata with AstReflection {
  val global: MacroUniverse
  lazy val g: global.type = global
  lazy val u: global.type = global
  lazy val mirror: u.Mirror = global.rootMirror
}

// TODO: quite probably we can relax the requirements from Global to MacroUniverse
// however that's not an immediate blocker, so I leave that for future work
trait GlobalToolkit extends MacroToolkit
                       with TreeHelpers
                       with TypeHelpers
                       with SymbolHelpers
                       with Platform
                       with LogicalSymbols
                       with LogicalTrees {
  val global: Global
  object l extends LogicalSymbols with LogicalTrees
}