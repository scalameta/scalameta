package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.org.scalameta.reflection.Helpers
import org.scalameta.unreachable

trait Syntaxize {
  self: MacroToolkit =>

  import global._
  import definitions._

  implicit class RichSyntaxizeType(tpe: Type) {
    def syntaxize: Tree = ???
  }
}