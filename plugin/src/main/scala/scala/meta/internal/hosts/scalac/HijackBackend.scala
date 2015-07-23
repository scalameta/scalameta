package scala.meta
package internal.hosts.scalac

import scala.collection.mutable
import scala.tools.nsc.{Global, SubComponent}
import scala.tools.nsc.backend.jvm.{GenBCode => NscGenBCode}
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}

trait HijackBackend {
  self: ScalahostPlugin =>

  // NOTE: mostly copy/pasted from https://github.com/VladimirNik/tasty/blob/7b45111d066ddbc43d859c9f6c0a81978111cf90/plugin/src/main/scala/scala/tasty/internal/scalac/Plugin.scala
  def hijackBackend(): (global.genBCode.type, NscGenBCode) = {
    if (self.global.settings.Ybackend.value == "GenBCode") {
      val oldBackend = global.genBCode
      object newBackend extends {
        override val global: self.global.type = self.global
      } with ScalahostGenBCode(global)

      val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
      genBCodeField.setAccessible(true)
      genBCodeField.set(global, newBackend)

      val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
      val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
      val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
      val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
      if (phasesSet.exists(_.phaseName.contains("jvm"))) { // `scalac -help` doesn't instantiate standard phases
        def subcomponentNamed(name: String) = phasesSet.find(_.phaseName == name).head
        val oldScs @ List(_) = List(subcomponentNamed("jvm"))
        val newScs = List(newBackend)
        def hijackDescription(pt: SubComponent, sc: SubComponent) = phasesDescMap(sc) = phasesDescMap(pt) + " with TASTY support"
        oldScs zip newScs foreach { case (pt, sc) => hijackDescription(pt, sc) }
        phasesSet --= oldScs
        phasesSet ++= newScs
      }

      (oldBackend, newBackend)
    } else {
      (global.genBCode, global.genBCode)
    }
  }
}