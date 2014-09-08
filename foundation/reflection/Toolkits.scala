package org.scalameta.reflection

import scala.reflect.macros.{Universe => MacroUniverse}
import scala.tools.nsc.Global
import scala.org.scalameta.reflection.Helpers

trait MacroToolkit extends Metadata {
  val global: MacroUniverse
}

// TODO: quite probably we can relax the requirements from Global to MacroUniverse
// however that's not an immediate blocker, so I leave that for future work
trait GlobalToolkit extends MacroToolkit
                       with Helpers
                       with Ensugar
                       with Syntaxize {
  val global: Global
}