package scala.meta

import scala.meta.internal.hosts.scalac.contexts.{Compiler => Compiler}
import scala.meta.internal.hosts.scalac.contexts.{Adapter => AdapterImpl}

trait Context extends SemanticContext with InteractiveContext

object Context {
  def apply(artifacts: Artifact*)(implicit resolver: Resolver): Context = {
    new AdapterImpl(Compiler(), Domain(artifacts: _*)) {
      override def toString = s"""Context(${artifacts.mkString(", ")})"""
    }
  }
}
