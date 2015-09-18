package org.scalameta.example

import scala.meta.internal.ast._
import scala.meta.Scalahost

trait CompileTime {
  val global: scala.tools.nsc.Global
  implicit val c = Scalahost.mkGlobalContext(global)

  def example(sources: List[Source]): Unit = {
    println(sources)
  }
}