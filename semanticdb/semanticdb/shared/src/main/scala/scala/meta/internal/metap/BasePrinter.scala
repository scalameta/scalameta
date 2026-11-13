package scala.meta.internal.metap

import scala.meta.cli._
import scala.meta.internal.{semanticdb => s}
import scala.meta.metap._

import java.io._

abstract class BasePrinter(
    val settings: Settings,
    val reporter: Reporter,
    val doc: s.TextDocument,
    val symtab: PrinterSymtab,
) {

  import s.Implicits._

  def out: PrintStream = reporter.out

  def rep[T](pre: String, xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit =
    if (xs.nonEmpty) {
      str(pre)
      rep(xs, sep)(f)
      str(suf)
    }

  def rep[T](pre: String, xs: Seq[T], sep: String)(f: T => Unit): Unit = rep(pre, xs, sep, "")(f)

  def rep[T](xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit = rep("", xs, sep, suf)(f)

  def rep[T](xs: Seq[T], sep: String)(f: T => Unit): Unit = {
    val iter = xs.iterator
    if (iter.hasNext) {
      f(iter.next())
      while (iter.hasNext) {
        str(sep)
        f(iter.next())
      }
    }
  }

  def opt[T](pre: String, xs: Option[T], suf: String)(f: T => Unit): Unit = xs.foreach { x =>
    str(pre)
    f(x)
    str(suf)
  }

  def opt[T](pre: String, xs: Option[T])(f: T => Unit): Unit = opt(pre, xs, "")(f)

  def opt[T](pre: String, xs: s.Type)(f: s.Type => Unit): Unit =
    opt(pre, if (xs.nonEmpty) Some(xs) else None)(f)

  def opt[T](pre: String, xs: s.Signature)(f: s.Signature => Unit): Unit =
    opt(pre, if (xs.nonEmpty) Some(xs) else None)(f)

  def opt[T](xs: Option[T], suf: String)(f: T => Unit): Unit = opt("", xs, suf)(f)

  def opt[T](xs: s.Type, suf: String)(f: s.Type => Unit): Unit =
    opt("", if (xs.nonEmpty) Some(xs) else None, suf)(f)

  def opt[T](xs: s.Signature, suf: String)(f: s.Signature => Unit): Unit =
    opt("", if (xs.nonEmpty) Some(xs) else None, suf)(f)

  def opt[T](xs: s.Type)(f: s.Type => Unit): Unit =
    opt("", if (xs.nonEmpty) Some(xs) else None, "")(f)

  def opt[T](xs: s.Signature)(f: s.Signature => Unit): Unit =
    opt("", if (xs.nonEmpty) Some(xs) else None, "")(f)

  def opt[T](xs: Option[T])(f: T => Unit): Unit = opt("", xs, "")(f)

  def opt(pre: String, s: String, suf: String)(f: String => Unit): Unit = if (s.nonEmpty) {
    str(pre)
    f(s)
    str(suf)
  }

  def opt(s: String, suf: String)(f: String => Unit): Unit = opt("", s, suf)(f)

  def opt(s: String)(f: String => Unit): Unit = opt("", s, "")(f)

  def str(value: String): Unit = out.print(value)

}
