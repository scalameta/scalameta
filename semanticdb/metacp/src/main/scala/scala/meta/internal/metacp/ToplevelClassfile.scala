package scala.meta.internal.metacp

import scala.meta.internal.io._
import scala.meta.io._
import scala.tools.asm.tree._

final case class ToplevelClassfile(
    base: AbsolutePath,
    path: AbsolutePath,
    node: ClassNode
) {
  def uri: String = PathIO.toUnix(path.toRelative(base).toString)
}
