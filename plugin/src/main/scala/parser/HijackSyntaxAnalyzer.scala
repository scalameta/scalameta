package scala.reflect.hosts
package scalacompiler
package parser

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.mutable
import scala.reflect.hosts.scalacompiler.parser.{SyntaxAnalyzer => PalladiumSyntaxAnalyzer}

trait HijackSyntaxAnalyzer {
  self: NscPlugin =>

  def hijackSyntaxAnalyzer(): Unit = {
    val syntaxAnalyzer = new { val global: self.global.type = self.global } with PalladiumSyntaxAnalyzer
    val syntaxAnalyzerField = classOf[Global].getDeclaredField("syntaxAnalyzer")
    syntaxAnalyzerField.setAccessible(true)
    syntaxAnalyzerField.set(global, syntaxAnalyzer)

    val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
    val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
    val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
    val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
    if (phasesSet.exists(_.phaseName == "parser")) { // `scalac -help` doesn't instantiate standard phases
      phasesSet -= phasesSet.find(_.phaseName == "parser").head
      phasesSet += syntaxAnalyzer
    }
  }
}