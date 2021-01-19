package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}

trait ParseOps { self: SemanticdbOps =>

  implicit class XtensionCompilationUnitSource(unit: g.CompilationUnit) {
    def toSource: m.Source =
      toSource(None)

    def toSource(explicitDialect: Option[m.Dialect]): m.Source = {
      val dialect =
        explicitDialect
          .orElse(m.Dialect.standards.get(language))
          .getOrElse(sys.error(s"unsupported dialect $language"))

      dialect(unit.source.toInput).parse[m.Source].get
    }
  }
}
