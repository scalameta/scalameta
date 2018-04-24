package scala.meta.internal

import java.nio.file._
import scala.meta.io.AbsolutePath
import scala.tools.asm._
import scala.tools.asm.ClassReader._
import scala.tools.asm.tree._

package object metacp {
  implicit class XtensionAsmPathOps(path: AbsolutePath) {
    def toClassNode: ClassNode = {
      val node = new ClassNode()
      val bytes = Files.readAllBytes(path.toNIO)
      new ClassReader(bytes).accept(node, SKIP_DEBUG | SKIP_FRAMES | SKIP_CODE)
      node
    }
  }
}
