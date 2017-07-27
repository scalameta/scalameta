package scala.meta.internal
package semantic

import scala.{meta => m}

trait ParseOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitSource(unit: g.CompilationUnit) {
    def toSource: m.Source = {
      val dialect = m.Dialect.standards.getOrElse(language, sys.error(s"unsupported dialect $language"))
      unit.cache.getOrElse("source", dialect(unit.source.toInput).parse[m.Source].get)
    }
  }
}
