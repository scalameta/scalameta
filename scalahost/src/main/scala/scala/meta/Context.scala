package scala.meta

import scala.meta.internal.hosts.scalac.contexts.{Compiler => Compiler}
import scala.meta.internal.hosts.scalac.contexts.{Proxy => ProxyImpl}

trait Context extends SemanticContext with InteractiveContext

object Context {
  def apply(artifacts: Artifact*)(implicit context: ArtifactContext): Context = {
    new ProxyImpl(Compiler(), Domain(artifacts: _*)) {
      override def toString = s"""Context(${artifacts.mkString(", ")})"""
    }
  }

  def apply(options: String, artifacts: Artifact*)(implicit context: ArtifactContext): Context = {
    new ProxyImpl(Compiler(options), Domain(artifacts: _*)) {
      override def toString = s"""Context("$options", ${artifacts.mkString(", ")})"""
    }
  }
}
