package scala.meta.internal
package scalahost
package databases

import scala.util.Properties
import scala.{meta => m}
import scala.reflect.io.{PlainFile => GPlainFile}

trait ParseOps {
  self: DatabaseOps =>

  def dialect: m.Dialect = {
    val version = Properties.versionNumberString
    if (version.startsWith("2.10")) scala.meta.dialects.Scala210
    else if (version.startsWith("2.11")) scala.meta.dialects.Scala211
    else if (version.startsWith("2.12")) scala.meta.dialects.Scala212
    else sys.error(s"unsupported Scala version $version")
  }

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
