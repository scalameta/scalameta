package scala.meta.internal
package scalahost
package v1

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.unreachable
import org.scalameta.debug
import scala.compat.Platform.EOL
import scala.util.Properties
import scala.meta._
import scala.meta.semantic.v1.Completed
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.{Mirror => MirrorApi}
import scala.meta.internal.ast.Helpers._

trait Mirror extends MirrorApi with LocationOps {
  def dialect: Dialect = {
    val version = Properties.versionNumberString
    if (version.startsWith("2.10")) scala.meta.dialects.Scala210
    else if (version.startsWith("2.11")) scala.meta.dialects.Scala211
    else if (version.startsWith("2.12")) scala.meta.dialects.Scala212
    else sys.error(s"unsupported Scala version $version")
  }

  // NOTE: Implemented differently by online and offline mirrors
  def sources: Seq[Source]

  // NOTE: Implemented differently by online and offline mirrors
  def database: Database

  def symbol(tree: Ref): Completed[Symbol] = apiBoundary {
    def relevantPosition(tree1: Tree): Position = tree1 match {
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
      case _ => unreachable(debug(tree1.syntax, tree1.structure))
    }
    val tree1 = if (isTypechecked(tree)) tree else typecheck(tree)
    val position = relevantPosition(tree1)
    val location = position.toSemantic
    database.names.getOrElse(location, sys.error(s"semantic DB doesn't contain $tree"))
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

  private def isTypechecked(tree: Tree): Boolean = {
    val indexedAddrs = database.names.keys.map(_.addr).toSet
    var allIndexed = true
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = {
        val addr = {
          try tree.pos.toSemantic.addr
          catch {
            case ex: Exception =>
              allIndexed = false
              return
          }
        }
        if (indexedAddrs(addr)) {
          super.apply(tree)
        } else {
          allIndexed = false
          return
        }
      }
    }
    traverser(tree.root)
    allIndexed
  }

  def typecheck(tree: Tree): Tree
}
