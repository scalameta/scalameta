package org.scalameta.meta

import scala.meta.internal.semantic._

trait DenotationHelpers {
  self: Toolkit =>

  implicit class RichRequireSymbolDenotation(denot: Denotation) {
    def requireSymbol: Symbol = denot.symbols match {
      case Nil => throw new Exception(s"no symbols in denotation $denot")
      case List(symbol) => symbol
      case List(head, rest @ _*) => throw new Exception(s"multiple symbols in denotation $denot: ${denot.symbols}")
    }
  }
}
