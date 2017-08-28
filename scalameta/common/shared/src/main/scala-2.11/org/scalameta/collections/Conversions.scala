package org.scalameta.collections

import scala.collection.immutable.{Map => ScalaMap}
import java.util.{HashMap => JavaMap}

private[collections] trait Conversions {
  implicit class XtensionScalaMap[T, U](scalaMap: ScalaMap[T, U]) {
    def toJavaMap: JavaMap[T, U] = {
      import scala.collection.JavaConversions._
      new JavaMap[T, U](mapAsJavaMap(scalaMap))
    }
  }

  implicit class XtensionJavaMap[T, U](javaMap: JavaMap[T, U]) {
    def toScalaMap: ScalaMap[T, U] = {
      import scala.collection.JavaConversions._
      javaMap.toMap
    }
  }
}
