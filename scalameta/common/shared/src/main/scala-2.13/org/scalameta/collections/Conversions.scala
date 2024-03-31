package org.scalameta.collections

import java.{util => ju}

import scala.collection.immutable._
import scala.collection.mutable

private[collections] trait Conversions {
  import scala.jdk.CollectionConverters._

  implicit class XtensionScalaMap[T, U](obj: Map[T, U]) {
    def toJava: ju.HashMap[T, U] = new ju.HashMap[T, U](obj.asJava)
  }

  implicit class XtensionScalaIterator[T](obj: Iterator[T]) {
    def toJava: ju.Iterator[T] = obj.asJava
  }

  implicit class XtensionJavaMap[T, U](obj: ju.HashMap[T, U]) {
    def toScala: Map[T, U] = obj.asScala.toMap
  }

  implicit class XtensionJavaList[T](obj: ju.List[T]) {
    def toScalaBuffer: mutable.Buffer[T] = obj.asScala
    def toScala: List[T] = toScalaBuffer.toList
  }

  implicit class XtensionJavaIterator[T](obj: ju.Iterator[T]) {
    def toScala: Iterator[T] = obj.asScala
  }
}
