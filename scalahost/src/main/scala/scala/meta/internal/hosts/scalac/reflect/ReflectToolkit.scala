package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global

trait ReflectToolkit extends Metadata
                        with TreeHelpers
                        with TypeHelpers
                        with SymbolHelpers
                        with Platform
                        with LogicalSymbols
                        with LogicalTrees {
  val global: Global
  lazy val g: global.type = global
  object l extends LogicalSymbols with LogicalTrees
}