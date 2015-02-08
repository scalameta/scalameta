package org.scalameta

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.nsc.Global

package object reflection {
  implicit class RichReflectionToolbox(tb: ToolBox[ru.type]) {
    def global: Global = {
      val m_withCompilerApi = tb.getClass.getDeclaredMethods.filter(_.getName == "withCompilerApi").head
      val withCompilerApi = m_withCompilerApi.invoke(tb)
      val m_api = withCompilerApi.getClass.getDeclaredMethods.filter(_.getName == "api").head
      m_api.setAccessible(true)
      val api = m_api.invoke(withCompilerApi)
      val m_compiler = api.getClass.getDeclaredMethods.filter(_.getName == "compiler").head
      m_compiler.invoke(api).asInstanceOf[scala.tools.nsc.Global]
    }
  }
}
