package scala.meta.internal

import scala.collection.mutable
import scala.tools.nsc.{Global => NscGlobal, SubComponent}
import scala.tools.nsc.typechecker.SemanticdbAnalyzer

trait HijackAnalyzer extends SemanticdbAnalyzer { self: SemanticdbPlugin =>

  def hijackAnalyzer(): Unit = {
    if (!isBatchCompiler) return

    val newAnalyzer = new { val global: self.global.type = self.global } with SemanticdbAnalyzer
    val analyzerField = classOf[NscGlobal].getDeclaredField("analyzer")
    analyzerField.setAccessible(true)
    analyzerField.set(global, newAnalyzer)

    val phasesSetMapGetter = classOf[NscGlobal].getDeclaredMethod("phasesSet")
    val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
    if (phasesSet.exists(_.phaseName == "typer")) { // `scalac -help` doesn't instantiate standard phases
      def subcomponentNamed(name: String) = phasesSet.find(_.phaseName == name).head
      val oldScs @ List(oldNamer, oldPackageobjects, oldTyper) = List(
        subcomponentNamed("namer"),
        subcomponentNamed("packageobjects"),
        subcomponentNamed("typer"))
      val newScs =
        List(newAnalyzer.namerFactory, newAnalyzer.packageObjects, newAnalyzer.typerFactory)
      def hijackDescription(pt: SubComponent, sc: SubComponent) = {
        val phasesDescMapGetter = classOf[NscGlobal].getDeclaredMethod("phasesDescMap")
        val phasesDescMap =
          phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
        phasesDescMap(sc) = phasesDescMap(pt)
      }
      oldScs zip newScs foreach { case (pt, sc) => hijackDescription(pt, sc) }
      phasesSet --= oldScs
      phasesSet ++= newScs
    }
  }
}
