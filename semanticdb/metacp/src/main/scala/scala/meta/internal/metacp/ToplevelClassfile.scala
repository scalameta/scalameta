package scala.meta.internal.metacp

import org.langmeta.internal.io._
import org.langmeta.io.AbsolutePath
import scala.tools.asm.tree._

final case class ToplevelClassfile(base: AbsolutePath, path: AbsolutePath, node: ClassNode) {
  def uri: String = PathIO.toUnix(path.toRelative(base).toString)
}
