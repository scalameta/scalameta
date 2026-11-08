package scala.meta.internal

import scala.annotation.tailrec

object NullableOps {

  implicit class ExtensionNullable[A >: Null <: AnyRef](private val obj: A) extends AnyVal {

    @inline
    def ??(or: => A): A = if (obj ne null) obj else or
    @inline
    def &&&[B >: Null](or: => B): B = if (obj eq null) null else or
    @inline
    def ===(or: => A): Boolean = (obj ne null) && (obj eq or)
    @inline
    def =!=(or: => A): Boolean = !(obj === or)
    @inline
    def nnIf(pred: A => Boolean): A = if ((obj ne null) && pred(obj)) obj else null
    @inline
    def orHas(pred: A => Boolean): Boolean = (obj eq null) || pred(obj)
    @inline
    def nnHas(pred: A => Boolean): Boolean = (obj ne null) && pred(obj)
    @inline
    def nnMap[B >: Null](f: A => B): B = if (obj eq null) null else f(obj)
    @inline
    def nnFor(f: A => Unit): Unit = if (obj ne null) f(obj)
    @inline
    def nnFold[B](or: => B)(f: A => B): B = if (obj eq null) or else f(obj)
    @inline
    def nnMapOr[B](f: A => B)(or: => B): B = if (obj eq null) or else f(obj)

  }

  implicit class ExtensionSeq[A >: Null <: AnyRef](private val obj: Seq[A]) extends AnyVal {

    @inline
    def headOrNull: A = if (obj.isEmpty) null else obj.head
    @inline
    def lastOrNull: A = if (obj.isEmpty) null else obj.last
  }

  implicit class ExtensionList[A >: Null <: AnyRef](private val obj: List[A]) extends AnyVal {

    @inline
    def ?::(elem: A): List[A] = if (elem eq null) obj else elem :: obj

    @inline @tailrec
    final def findOrNull(pred: A => Boolean): A = obj match {
      case head :: tail => if (pred(head)) head else tail.findOrNull(pred)
      case _ => null
    }
  }

}
