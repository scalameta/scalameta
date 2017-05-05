package scala.meta.internal
package semantic

import scala.tools.nsc.Global

trait DatabaseOps
    extends AttributesOps
    with DenotationOps
    with DialectOps
    with InputOps
    with ParseOps
    with ReporterOps
    with SymbolOps
    with ReflectionToolkit {
  val global: Global
}
