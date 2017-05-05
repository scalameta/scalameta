package scala.meta.internal
package semantic

import scala.{meta => m}
import scala.reflect.io.{PlainFile => GPlainFile}

trait ParseOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitSource(unit: g.CompilationUnit) {
    def toSource: m.Source = {
      unit.cache.getOrElse("source", {
        // TODO: Need to trim `unit.source.content` because scalac
        // always ensures a newline at the end of its compilation units.
        val input = unit.source.file match {
          case gplainFile: GPlainFile => m.Input.File(gplainFile.file)
          case _ => m.Input.String(new String(unit.source.content).trim)
        }
        dialect(input).parse[m.Source].get
      })
    }
  }
}
