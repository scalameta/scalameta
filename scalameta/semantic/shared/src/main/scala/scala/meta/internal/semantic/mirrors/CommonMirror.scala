package scala.meta
package internal
package semantic
package mirrors

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.unreachable
import org.scalameta.debug
import scala.compat.Platform.EOL
import scala.meta.inputs._
import scala.meta.internal.ast.Helpers._
import scala.meta.prettyprinters._
import scala.meta.semantic._

trait CommonMirror extends Mirror {
  // NOTE: Implemented differently by online and offline mirrors
  def dialect: Dialect

  // NOTE: Implemented differently by online and offline mirrors
  def sources: Seq[Source]

  // NOTE: Implemented differently by online and offline mirrors
  def database: Database

  def symbol(tree: Ref): Completed[Symbol] = apiBoundary {
    def relevantPosition(tree: Tree): Position = tree match {
      case name1: Name => name1.pos
      case _: Term.This => ???
      case _: Term.Super => ???
      case Term.Select(_, name1) => name1.pos
      case Term.ApplyUnary(_, name1) => name1.pos
      case Type.Select(_, name1) => name1.pos
      case Type.Project(_, name1) => name1.pos
      case Type.Singleton(ref1) => relevantPosition(ref1)
      case Ctor.Ref.Select(_, name1) => name1.pos
      case Ctor.Ref.Project(_, name1) => name1.pos
      case Ctor.Ref.Function(name1) => ???
      case _: Importee.Wildcard => ???
      case Importee.Name(name1) => name1.pos
      case Importee.Rename(name1, _) => name1.pos
      case Importee.Unimport(name1) => name1.pos
      case _ => unreachable(debug(tree.syntax, tree.structure))
    }
    val position = relevantPosition(tree)
    val anchor: Anchor = {
      // TODO: This is only going to work well if we embed file contents.
      // The corresponding commit should land pretty soon.
      ???
    }
    database.names.getOrElse(anchor, sys.error(s"semantic DB doesn't contain $tree"))
  }

  def denot(sym: Symbol): Completed[Denotation] = apiBoundary {
    database.denotations.getOrElse(sym, sys.error(s"semantic DB doesn't contain $sym"))
  }

  private def apiBoundary[T](op: => T): Completed[T] = {
    try {
      val result = op
      Completed.Success(result)
    } catch {
      case ex: SemanticException =>
        Completed.Error(ex)
      case ex: Exception =>
        var message = s"fatal error: ${ex.getMessage}$EOL"
        message += "This is a bug; please report it via https://github.com/scalameta/scalameta/issues/new."
        Completed.Error(new SemanticException(Position.None, message, Some(ex)))
    }
  }
}
