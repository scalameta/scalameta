package scala.meta.internal.metacp

import org.langmeta.internal.io._
import org.langmeta.io.AbsolutePath
import org.langmeta.io.RelativePath

import scala.tools.asm.tree._

final case class ToplevelClassfile(base: AbsolutePath, path: AbsolutePath, node: ClassNode) {
  def relative: RelativePath = path.toRelative(base)
  def name: String = {
    PathIO.toUnix(path.toRelative(base).toString)
  }
}
