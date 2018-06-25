package scala.meta.internal

import java.nio.file.Files
import scala.meta.io.AbsolutePath
import scala.tools.asm._
import scala.tools.asm.ClassReader._
import scala.tools.asm.tree._

package object metacp {
  private val SKIP_ALL = SKIP_CODE | SKIP_DEBUG | SKIP_FRAMES

  implicit class XtensionAsmBytes(entry: ReadableClasspathEntry) {
    def hasScalaSig: Boolean = {
      val visitor = new HasScalaSigVisitor
      val in = entry.openInputStream()
      try {
        new ClassReader(in).accept(visitor, SKIP_ALL)
        visitor.hasScalaSig
      } finally {
        in.close()
      }
    }
  }

  implicit class XtensionAsmPathOps(path: AbsolutePath) {
    def toClassNode: ClassNode = {
      val node = new ClassNode()
      val in = Files.newInputStream(path.toNIO)
      try {
        new ClassReader(in).accept(node, SKIP_ALL)
        node
      } finally {
        in.close()
      }
    }
  }
}
