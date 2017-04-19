package scala.meta.internal
package scalahost
package v1
package online

import java.util.UUID.randomUUID
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.util.Properties
import scala.reflect.io.{VirtualFile => GVirtualFile}
import scala.reflect.internal.util.{BatchSourceFile => GBatchSourceFile}
import scala.tools.nsc.reporters.{StoreReporter => GStoreReporter}
import scala.tools.nsc.Global
import scala.meta._
import scala.meta.semantic.v1.{Mirror => MirrorApi}
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.scalahost.v1.{Mirror => BaseMirror}

class Mirror(val global: Global)
    extends MirrorApi
    with BaseMirror
    with DatabaseOps
    with DenotationOps
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

  def sources: Seq[Source] = {
    units.toList.map(_.toSource)
  }

  private var cachedDatabaseKey = (g.currentRun, -1)
  private def recomputeCachedDatabaseKey() = (g.currentRun, g.currentRun.size)
  private var cachedDatabase: Database = null
  def database: Database = {
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
              Database()
          }
        })
        if (unmappedNames != "") sys.error(unmappedNames.trim)
        val names = databases.flatMap(_.names).toMap
        val messages = databases.flatMap(_.messages)
        val denotations = databases.flatMap(_.denotations).toMap
        Database(names, messages, denotations)
      }
      cachedDatabaseKey = recomputeCachedDatabaseKey()
      cachedDatabase = database
    }
    cachedDatabase
  }
}
