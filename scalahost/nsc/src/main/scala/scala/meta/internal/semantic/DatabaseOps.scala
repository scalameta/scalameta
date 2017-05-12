package scala.meta.internal
package semantic

import scala.tools.nsc.Global
import scala.meta.io._

trait DatabaseOps
    extends AttributesOps
    with ConfigOps
    with DenotationOps
    with DialectOps
    with InputOps
    with ParseOps
    with ReporterOps
    with SymbolOps
    with ReflectionToolkit {
  val global: Global
}
