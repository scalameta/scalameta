package scala.meta.internal
package scalahost
package v1

import org.scalameta.unreachable
import org.scalameta.debug
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.util.Properties
import scala.reflect.io.PlainFile
import scala.tools.nsc.Global
import scala.{meta => m}
import scala.meta.semantic.{v1 => mv1}

class OnlineMirror(val global: Global) extends mv1.Mirror
                                          with DatabaseOps
                                          with DialectOps
                                          with GlobalOps {

  override def toString: String = {
    val compiler = s"the Scala compiler ${Properties.versionString}"
    val settings = global.settings.toConciseString
    s"online mirror for $compiler running with $settings"
  }

  lazy val sources: Seq[m.Source] = {
    g.currentRun.units.toList.map(unit => {
      val jfile = unit.source.file.file
      if (jfile == null) sys.error("Unsupported compilation unit with abstract file ${unit.source.file}")
      dialect(jfile).parse[m.Source].get
    })
  }

  lazy val database: mv1.Database = {
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

  def symbol(ref: m.Ref): mv1.Completed[mv1.Symbol] = {
    def relevantPosition(ref: m.Ref): m.Position = ref match {
      case name: m.Name => ref.pos
      case _: m.Term.This => ???
      case _: m.Term.Super => ???
      case m.Term.Select(_, name) => name.pos
      case m.Term.ApplyUnary(_, name) => name.pos
      case m.Type.Select(_, name) => name.pos
      case m.Type.Project(_, name) => name.pos
      case m.Type.Singleton(ref) => relevantPosition(ref)
      case m.Ctor.Ref.Select(_, name) => name.pos
      case m.Ctor.Ref.Project(_, name) => name.pos
      case m.Ctor.Ref.Function(name) => ???
      case _: m.Importee.Wildcard => ???
      case m.Importee.Name(name) => name.pos
      case m.Importee.Rename(name, _) => name.pos
      case m.Importee.Unimport(name) => name.pos
      case _ => unreachable(debug(ref.syntax, ref.structure))
    }
    val position = relevantPosition(ref)
    val location = position.toSemantic
    database.symbols.get(location) match {
      case Some(symbol) =>
        mv1.Completed.Success(symbol)
      case _ =>
        val message = s"failed to resolve $ref in $this"
        mv1.Completed.Error(m.SemanticException(ref.pos, message))
    }
  }
}