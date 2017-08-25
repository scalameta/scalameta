package scala.meta.internal
package semanticdb

import scala.tools.nsc.Global

trait DatabaseOps
    extends SourceFileOps
    with ConfigOps
    with DefinitionOps
    with InputOps
    with LanguageOps
    with ParseOps
    with ReporterOps
    with PrinterOps
    with SymbolOps
    with MessageOps
    with ReflectionToolkit {
  val global: Global
}
