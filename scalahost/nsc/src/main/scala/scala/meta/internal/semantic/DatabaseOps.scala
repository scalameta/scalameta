package scala.meta.internal
package semantic

import scala.tools.nsc.Global

trait DatabaseOps
    extends AttributedSourceOps
    with DenotationOps
    with ParseOps
    with PositionOps
    with ReporterOps
    with SymbolOps
    with ReflectionToolkit {
  val global: Global
}
