package scala.meta.internal
package scalahost
package v1

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.tools.nsc.Global
import scala.util.Properties
import scala.{meta => m}
import scala.meta.internal.semantic.v1.mirrors.CommonMirror

class OnlineMirror(val global: Global)
    extends CommonMirror
    with DatabaseOps
    with DenotationOps
    with LocationOps
    with ParseOps
    with ReporterOps
    with SymbolOps
    with ReflectionToolkit {

  override def toString: String = {
    val compiler = s"the Scala compiler ${Properties.versionString}"
    val settings = global.settings.toConciseString
    s"online mirror for $compiler running with $settings"
  }

  private def units: Seq[g.CompilationUnit] = {
    g.currentRun.units.filter(!_.source.file.name.endsWith(".java")).toList
  }

  def sources: Seq[m.Source] = {
    units.toList.map(_.toSource)
  }

  private var cachedDatabaseKey = (g.currentRun, -1)
  private def recomputeCachedDatabaseKey() = (g.currentRun, g.currentRun.size)
  private var cachedDatabase: m.Database = null
  def database: m.Database = {
    // NOTE: We rely on the fact that compilation units change monotonously,
    // i.e. that we can only add new compilation units, but not remove them.
    if (cachedDatabaseKey != recomputeCachedDatabaseKey()) {
      val database = {
        var unmappedNames = ""
        val databases = units.map(unit => {
          try unit.toDatabase
          catch {
            case ex: Exception if ex.getMessage.startsWith("Unmapped names in") =>
              unmappedNames += (ex.getMessage + EOL)
              m.Database()
          }
        })
        if (unmappedNames != "") sys.error(unmappedNames.trim)
        val names = databases.flatMap(_.names).toMap
        val messages = databases.flatMap(_.messages)
        val denotations = databases.flatMap(_.denotations).toMap
        m.Database(names, messages, denotations)
      }
      cachedDatabaseKey = recomputeCachedDatabaseKey()
      cachedDatabase = database
    }
    cachedDatabase
  }
}
