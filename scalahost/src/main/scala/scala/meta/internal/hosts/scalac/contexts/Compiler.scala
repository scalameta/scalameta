package scala.meta
package internal.hosts.scalac
package contexts

import scala.tools.nsc.Global
import scala.meta.taxonomic.{Context => TaxonomicContext}

object Compiler {
  def apply(module: Module*)(implicit taxonomy: TaxonomicContext): Global = {
    apply("", module: _*)
  }

  def apply(options: String, module: Module*)(implicit taxonomy: TaxonomicContext): Global = {
    // TODO: Create an instance of Global based on compiler options and a list of modules.
    // When processing modules, typecheck the sources of adhoc modules and load the sources of artifacts.
    ???
  }
}