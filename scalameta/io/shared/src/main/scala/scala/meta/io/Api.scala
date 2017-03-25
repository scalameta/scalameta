package scala.meta.io

import scala.meta.internal.io._

private[meta] trait Api {
  implicit class XtensionAbsolutePath(path: AbsolutePath) {
    def slurp: String = PlatformIO.slurp(path)
  }
}

private[meta] trait Aliases {
  type AbsolutePath = scala.meta.io.AbsolutePath
  lazy val AbsolutePath = scala.meta.io.AbsolutePath
}
