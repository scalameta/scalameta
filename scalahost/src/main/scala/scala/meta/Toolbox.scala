package scala.meta

import scala.meta.semantic.{Context => SemanticContext}
import scala.meta.interactive.{Context => InteractiveContext}
import scala.meta.taxonomic.{Context => TaxonomicContext}
import scala.meta.internal.hosts.scalac.contexts.{Compiler => Compiler}
import scala.meta.internal.hosts.scalac.contexts.{Proxy => ProxyImpl}

object Toolbox {
  def apply(modules: Module*)(implicit taxonomy: TaxonomicContext): Toolbox = {
    val DefaultTaxonomy = "shadow scalahost's default taxonomy"
    new ProxyImpl(Compiler(modules: _*), Domain(modules: _*))
  }

  def apply(options: String, modules: Module*)(implicit taxonomy: TaxonomicContext): Toolbox = {
    val DefaultTaxonomy = "shadow scalahost's default taxonomy"
    new ProxyImpl(Compiler(options, modules: _*), Domain(modules: _*))
  }
}

trait Toolbox extends Mirror with SemanticContext with InteractiveContext
