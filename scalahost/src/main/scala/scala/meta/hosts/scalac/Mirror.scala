package scala.meta
package hosts.scalac

import scala.meta.semantic.{Context => SemanticContext}
import scala.meta.taxonomic.{Context => TaxonomicContext}
import scala.meta.internal.hosts.scalac.contexts.{Compiler => CompilerImpl}
import scala.meta.internal.hosts.scalac.contexts.{Proxy => ProxyImpl}

object Mirror {
  def apply(modules: Module*)(implicit taxonomy: TaxonomicContext): Mirror = {
    // TODO: In the future, we may avoid instantiating the entire compiler here,
    // because a mirror can theoretically be built on top of scala.reflect.runtime.universe
    // that supposedly starts up faster.
    val DefaultTaxonomy = "shadow scalahost's default taxonomy"
    new ProxyImpl(CompilerImpl(modules: _*))
  }
}

trait Mirror extends SemanticContext
