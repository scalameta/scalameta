package scala.meta
package hosts.scalac

import scala.meta.semantic.{Context => SemanticContext}
import scala.meta.interactive.{Context => InteractiveContext}
import scala.meta.taxonomic.{Context => TaxonomicContext}
import scala.meta.internal.hosts.scalac.contexts.{Compiler => CompilerImpl}
import scala.meta.internal.hosts.scalac.contexts.{Proxy => ProxyImpl}

object Toolbox {
  def apply(modules: Module*)(implicit taxonomy: TaxonomicContext): Toolbox = {
    val DefaultTaxonomy = "shadow scalahost's default taxonomy"
    new ProxyImpl(CompilerImpl(modules: _*))
  }

  def apply(options: String, modules: Module*)(implicit taxonomy: TaxonomicContext): Toolbox = {
    val DefaultTaxonomy = "shadow scalahost's default taxonomy"
    new ProxyImpl(CompilerImpl(options, modules: _*))
  }
}

trait Toolbox extends Mirror with SemanticContext with InteractiveContext
