package scala.meta.internal.io

import java.io._

trait OutputStreamIO[A] {
  def write(obj: A, os: OutputStream): Unit
}
