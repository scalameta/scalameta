package scala.meta
package internal.hosts.scalac
package contexts

import java.io.File
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.taxonomic.{Context => TaxonomicContext}

object Compiler {
  def apply(modules: Module*)(implicit taxonomy: TaxonomicContext): Global = {
    apply("", modules: _*)
  }

  // TODO: Theoretically, here we could just do `modules.flatMap(_.sources)`,
  // then do `modules.flatMap(_.deps)`, then put the dependencies on the compilation classpath,
  // convert all sources to scala.reflect, load them into the compiler and live happily ever after.
  // However, as things currently stand, it is going to be quite wasteful performance-wise
  // to load meta trees and convert them to reflect trees eagerly.
  // Also, the meta => reflect converter doesn't work yet. Therefore, we will have to think of something smarter.
  def apply(options: String, modules: Module*)(implicit taxonomy: TaxonomicContext): Global = {
    ???
  }
}