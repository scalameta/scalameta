package scala.meta.internal.metacp

import java.nio.file._
import org.langmeta.internal.io._
import scala.tools.asm.tree._

final case class ToplevelClassfile(base: Path, path: Path, node: ClassNode) {
  def name: String = {
    PathIO.toUnix(base.relativize(path).toString)
  }
}
