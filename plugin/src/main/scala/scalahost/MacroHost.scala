package scala.meta
package internal.hosts.scalacompiler
package scalahost

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.semantic._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.macros.contexts.{Context => ScalaMacroContext}
import scala.meta.semantic.{MacroHost => PalladiumMacroHost}

class MacroHost[G <: ScalaGlobal](val c: ScalaMacroContext) extends Host[G](c.universe.asInstanceOf[G]) with PalladiumMacroHost {
  def warning(msg: String): Unit = ???
  def error(msg: String): Unit = ???
  def abort(msg: String): Nothing = ???
  def resources: Seq[String] = ???
  def resource(url: String): Array[Byte] = ???
}