package scala.meta.internal.metacp

import scala.meta.cli.Reporter
import scala.meta.internal.io._
import scala.meta.io._
import scala.meta.metacp.Settings
import scala.tools.asm.tree._

final case class ToplevelClassfile(
    base: AbsolutePath,
    path: AbsolutePath,
    node: ClassNode,
    index: ClasspathIndex,
    settings: Settings,
    reporter: Reporter
) {
  def uri: String = PathIO.toUnix(path.toRelative(base).toString)
}
