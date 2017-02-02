package scala.meta.internal
package scalahost
package v1

import org.scalameta.unreachable
import org.scalameta.debug
import scala.meta._
import scala.meta.semantic.v1.Completed
import scala.meta.semantic.v1.Database

trait MirrorOps extends LocationOps { self: Mirror =>

  def symbol(ref: Ref): Completed[Symbol] = {
    def relevantPosition(ref: Ref): Position = ref match {
      case name: Name                => ref.pos
      case _: Term.This              => ???
      case _: Term.Super             => ???
      case Term.Select(_, name)      => name.pos
      case Term.ApplyUnary(_, name)  => name.pos
      case Type.Select(_, name)      => name.pos
      case Type.Project(_, name)     => name.pos
      case Type.Singleton(ref)       => relevantPosition(ref)
      case Ctor.Ref.Select(_, name)  => name.pos
      case Ctor.Ref.Project(_, name) => name.pos
      case Ctor.Ref.Function(name)   => ???
      case _: Importee.Wildcard      => ???
      case Importee.Name(name)       => name.pos
      case Importee.Rename(name, _)  => name.pos
      case Importee.Unimport(name)   => name.pos
      case _                         => unreachable(debug(ref.syntax, ref.structure))
    }
    val position = relevantPosition(ref)
    val location = position.toSemantic
    database.symbols.get(location) match {
      case Some(symbol) =>
        Completed.Success(symbol)
      case _ =>
        val message = s"failed to resolve $ref in $this"
        Completed.Error(SemanticException(ref.pos, message))
    }
  }
}
