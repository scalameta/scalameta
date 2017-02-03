package scala.meta.internal
package scalahost
package v1

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.util.Properties
import scala.reflect.io.PlainFile
import scala.tools.nsc.Global
import scala.{meta => m}
import scala.meta.semantic.{v1 => mv1}

class OnlineMirror(val global: Global)
    extends mv1.Mirror
    with DatabaseOps
    with DialectOps
    with GlobalOps
    with MirrorOps
    with ParseOps {

  override def toString: String = {
    val compiler = s"the Scala compiler ${Properties.versionString}"
    val settings = global.settings.toConciseString
    s"online mirror for $compiler running with $settings"
  }

  def sources: Seq[m.Source] = {
    g.currentRun.units.toList.map(_.toSource)
  }

  def database: mv1.Database = {
    var unmappedNames = ""
    val databases = g.currentRun.units.toList.map(unit => {
      try unit.toDatabase
      catch {
        case ex: Exception if ex.getMessage.startsWith("Unmapped names in") =>
          unmappedNames += (ex.getMessage + EOL)
          mv1.Database(Map())
      }
    })
    if (unmappedNames != "") sys.error(unmappedNames.trim)
    val symbols = databases.flatMap(_.symbols).toMap
    mv1.Database(symbols)
  }
}
