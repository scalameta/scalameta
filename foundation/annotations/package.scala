package org.scalareflect

import scala.annotation.StaticAnnotation

package object annotations {
  object internal {
    class ast extends StaticAnnotation
    class contextful[T] extends StaticAnnotation
    class hosted(macroApi: Boolean, mayFail: Boolean) extends StaticAnnotation
  }
}
