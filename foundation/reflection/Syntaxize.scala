package org.scalameta.reflection

trait Syntaxize {
  self: GlobalToolkit =>

  import global._
  import definitions._

  implicit class RichSyntaxizeType(tpe: Type) {
    def syntaxize: Tree = ???
  }
}