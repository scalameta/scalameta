package scala.meta
package internal.hosts.scalacompiler
package macros

import scala.tools.nsc.{Global => NscGlobal, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.mutable
import macros.{Analyzer => PalladiumAnalyzer}
import scalacompiler.{Plugin => PalladiumPlugin}
import scala.tools.nsc.interpreter.{ReplGlobal => NscReplGlobal}

trait HijackAnalyzer {
  self: PalladiumPlugin =>

  def hijackAnalyzer(): Unit = {
    // NOTE: need to hijack the right `analyzer` field - it's different for batch compilers and repl compilers
    val isRepl = global.isInstanceOf[NscReplGlobal]
    val analyzer = new { val global: self.global.type = self.global } with PalladiumAnalyzer {
      override protected def findMacroClassLoader(): ClassLoader = {
        val loader = super.findMacroClassLoader
        if (isRepl) {
          macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(global.classPath.asURLs))
          val virtualDirectory = global.settings.outputDirs.getSingleOutput.get
          new scala.reflect.internal.util.AbstractFileClassLoader(virtualDirectory, loader) {}
        } else {
          loader
        }
      }
    }
    val analyzerField = (if (isRepl) global.getClass else classOf[NscGlobal]).getDeclaredField("analyzer")
    analyzerField.setAccessible(true)
    analyzerField.set(global, analyzer)

    val phasesSetMapGetter = classOf[NscGlobal].getDeclaredMethod("phasesSet")
    val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
    if (phasesSet.exists(_.phaseName == "typer")) { // `scalac -help` doesn't instantiate standard phases
      def subcomponentNamed(name: String) = phasesSet.find(_.phaseName == name).head
      val oldScs @ List(oldNamer, oldPackageobjects, oldTyper) = List(subcomponentNamed("namer"), subcomponentNamed("packageobjects"), subcomponentNamed("typer"))
      val newScs = List(analyzer.namerFactory, analyzer.packageObjects, analyzer.typerFactory)
      phasesSet --= oldScs
      phasesSet ++= newScs
    }
  }
}