package scala.meta.internal.semanticdb.scalac

import scala.tools.nsc.Global

trait SemanticdbOps
    extends AnnotationOps
    with SymbolInformationOps
    with TextDocumentOps
    with InputOps
    with LanguageOps
    with DiagnosticOps
    with ParseOps
    with ReporterOps
    with ReflectionToolkit
    with SymbolOps
    with SyntheticOps
    with TypeOps {
  val global: Global
  var config: SemanticdbConfig = SemanticdbConfig.default
}
