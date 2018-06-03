package scala.meta.internal.metap

import java.io._
import scala.meta.cli._
import scala.meta.internal.semanticdb3._
import scala.meta.metap._

abstract class BasePrinter(val settings: Settings, val reporter: Reporter, val doc: TextDocument) {
  def out: PrintStream = {
    reporter.out
  }

  def rep[T](pre: String, xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      out.print(pre)
      rep(xs, sep)(f)
      out.print(suf)
    }
  }

  def rep[T](pre: String, xs: Seq[T], sep: String)(f: T => Unit): Unit = {
    rep(pre, xs, sep, "")(f)
  }

  def rep[T](xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit = {
    rep("", xs, sep, suf)(f)
  }

  def rep[T](xs: Seq[T], sep: String)(f: T => Unit): Unit = {
    xs.zipWithIndex.foreach {
      case (x, i) =>
        if (i != 0) out.print(sep)
        f(x)
    }
  }

  def opt[T](pre: String, xs: Option[T], suf: String)(f: T => Unit): Unit = {
    xs.foreach { x =>
      out.print(pre)
      f(x)
      out.print(suf)
    }
  }

  def opt[T](pre: String, xs: Option[T])(f: T => Unit): Unit = {
    opt(pre, xs, "")(f)
  }

  def opt[T](xs: Option[T], suf: String)(f: T => Unit): Unit = {
    opt("", xs, suf)(f)
  }

  def opt[T](xs: Option[T])(f: T => Unit): Unit = {
    opt("", xs, "")(f)
  }

  def opt(pre: String, s: String, suf: String)(f: String => Unit): Unit = {
    if (s.nonEmpty) {
      out.print(pre)
      f(s)
      out.print(suf)
    }
  }

  def opt(s: String, suf: String)(f: String => Unit): Unit = {
    opt("", s, suf)(f)
  }

  def opt(s: String)(f: String => Unit): Unit = {
    opt("", s, "")(f)
  }
}
