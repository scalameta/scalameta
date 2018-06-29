package scala.meta.internal

import java.io.InputStream
import java.nio.file.Files
import scala.meta.io.AbsolutePath
import scala.tools.asm._
import scala.tools.asm.ClassReader._
import scala.tools.asm.tree._
import scala.collection.JavaConverters._

package object metacp {
  implicit class XtensionAsmPathOps(path: AbsolutePath) {
    def toClassNode: ClassNode = {
      readInputStreamToClassNode(Files.newInputStream(path.toNIO))
    }
  }
  implicit class XtensionAsmClassfileOps(classfile: Classfile) {
    def hasScalaSig: Boolean = {
      val classNode = readInputStreamToClassNode(classfile.openInputStream())
      classNode.attrs != null && classNode.attrs.asScala.exists(_.`type` match {
        case "Scala" | "ScalaSig" => true
        case _ => false
      })
    }
  }
  private def readInputStreamToClassNode(in: InputStream): ClassNode = {
    val node = new ClassNode()
    try {
      new ClassReader(in).accept(node, SKIP_CODE | SKIP_DEBUG | SKIP_FRAMES)
      node
    } finally {
      in.close()
    }
  }
}
