package org.scalameta.internal

import scala.reflect.macros.blackbox.Context

trait MacroCompat {
  val c: Context
  import c.universe._
  val AssignOrNamedArg = NamedArg
  type AssignOrNamedArg = NamedArg
}

object MacroCompat {
  val productFieldNamesAvailable = true
}
