package scala.meta.internal
package scalahost
package databases

import scala.tools.nsc.Global

trait DatabaseOps
    extends AnchorOps
    with AttributedSourceOps
    with DenotationOps
    with ParseOps
    with ReporterOps
    with SymbolOps
    with ReflectionToolkit {
  val global: Global
}
