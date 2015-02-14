package org.scalameta.reflection

import scala.reflect.macros.{Universe => MacroUniverse}
import scala.tools.nsc.Global
import scala.org.scalameta.reflection.TreeHelpers

trait MacroToolkit extends Metadata {
  val global: MacroUniverse
  lazy val g: global.type = g
}

// TODO: quite probably we can relax the requirements from Global to MacroUniverse
// however that's not an immediate blocker, so I leave that for future work
trait GlobalToolkit extends MacroToolkit
                       with TreeHelpers
                       with TypeHelpers
                       with SymbolHelpers
                       with Ensugar
                       with Syntaxize
                       with Attributed
                       with Platform
                       with LogicalSymbols {
  val global: Global
}