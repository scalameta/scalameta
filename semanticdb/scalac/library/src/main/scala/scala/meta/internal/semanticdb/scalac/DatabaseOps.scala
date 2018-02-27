package scala.meta.internal.semanticdb.scalac

import scala.tools.nsc.Global

trait DatabaseOps
    extends AnnotationOps
    with DenotationOps
    with DocumentOps
    with InputOps
    with LanguageOps
    with MemberOps
    with MessageOps
    with NameOps
    with ParseOps
    with PrinterOps
    with ReporterOps
    with ReflectionToolkit
    with SymbolOps
    with TypeOps {
  val global: Global
  var config: SemanticdbConfig = SemanticdbConfig.default
}
