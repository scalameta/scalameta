package org.scalameta.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context

object internal {
  class contextful[T] extends StaticAnnotation
  class hosted(macroApi: Boolean) extends StaticAnnotation
}
