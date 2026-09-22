package org.scalameta.adt

import org.scalameta.internal.MacroHelpers

import scala.language.experimental.macros

abstract class GenerateHelper extends MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: c.mirror.type = c.mirror
  import c.universe._

  protected def getNameSuffix(name: String) = name.stripPrefix("scala.meta.")
  protected def getFullName(sym: Symbol) = getNameSuffix(sym.fullName)

  // Helper to build a List[String] tree from a Scala List[String]
  protected def mkStringList(parts: String*): c.Expr[List[String]] = {
    // a class file holds a string constant of at most 65535 bytes
    val literals = parts.map { s =>
      val chunks = s.grouped(16384).map(x => Literal(Constant(x))).toList
      q"_root_.scala.Seq(..$chunks).mkString"
    }
    c.Expr[List[String]](q"_root_.scala.List(..$literals)")
  }
}
