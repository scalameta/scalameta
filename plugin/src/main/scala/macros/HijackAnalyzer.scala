package scala.reflect.hosts
package scalacompiler
package macros

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.mutable
import scala.reflect.hosts.scalacompiler.{Plugin => PalladiumPlugin}
import scala.reflect.hosts.scalacompiler.macros.{Analyzer => PalladiumAnalyzer}

trait HijackAnalyzer {
  self: PalladiumPlugin =>

  def hijackAnalyzer(): Unit = {
    val analyzer = new { val global: self.global.type = self.global; val plugin: self.type = self } with PalladiumAnalyzer
    val analyzerField = classOf[Global].getDeclaredField("analyzer")
    analyzerField.setAccessible(true)
    analyzerField.set(global, analyzer)

    val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
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