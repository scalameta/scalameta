package scala.meta.internal

import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util
import scala.meta.internal.classpath._
import scala.meta.io.AbsolutePath
import scala.tools.asm._
import scala.tools.asm.ClassReader._
import scala.tools.asm.tree._
import scala.collection.JavaConverters._
import scala.meta.internal.scalacp.ScalaSigAttribute
import scala.meta.internal.scalacp.ScalaSigNode
import scala.reflect.internal.pickling.ByteCodecs
import scala.tools.scalap.Main
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.tools.scalap.scalax.rules.scalasig.ScalaSig
import scala.tools.scalap.scalax.rules.scalasig.ScalaSigAttributeParsers

package object metacp {
  implicit class XtensionClassNode(node: ClassNode) {
    def scalaSig: Option[ScalaSigNode] = {
      if (node.attrs == null) None
      else {
        for {
          scalaSigAttribute <- node.attrs.asScala.collectFirst {
            case ScalaSigAttribute(scalaSig) => scalaSig
          }
          scalaSig <- {
            if (scalaSigAttribute.table.nonEmpty) Some(scalaSigAttribute)
            else fromScalaSigAnnotation
          }
        } yield ScalaSigNode(node.name + ".class", scalaSig)
      }
    }
    private def fromScalaSigAnnotation: Option[ScalaSig] = {
      if (node.visibleAnnotations == null) None
      else {
        node.visibleAnnotations.asScala.collectFirst {
          case annot
              if annot.desc == Main.SCALA_SIG_ANNOTATION ||
                annot.desc == Main.SCALA_LONG_SIG_ANNOTATION =>
            annot.values.asScala match {
              case Seq("bytes", anyBytes) =>
                val baos = new ByteArrayOutputStream()
                val bytes: Array[Byte] = anyBytes match {
                  case bytesString: String =>
                    bytesString.getBytes(StandardCharsets.UTF_8)
                  case bytesArray: util.ArrayList[_] =>
                    bytesArray.asScala.foreach {
                      case bytesString: String =>
                        baos.write(bytesString.getBytes(StandardCharsets.UTF_8))
                    }
                    baos.toByteArray
                  case els => throw new IllegalArgumentException(els.getClass.getName)
                }
                val length = ByteCodecs.decode(bytes)
                val bytecode = ByteCode(bytes.take(length))
                ScalaSigAttributeParsers.parse(bytecode)
            }
        }
      }
    }

  }
  implicit class XtensionAsmPathOps(path: AbsolutePath) {
    def toClassNode: ClassNode = {
      readInputStreamToClassNode(Files.newInputStream(path.toNIO))
    }
  }

  implicit class XtensionAsmClassfileOps(classfile: Classfile) {
    def toClassNode: ClassNode = {
      readInputStreamToClassNode(classfile.openInputStream())
    }
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
      new ClassReader(in).accept(
        node,
        Array(ScalaSigAttribute),
        SKIP_CODE | SKIP_DEBUG | SKIP_FRAMES
      )
      node
    } finally {
      in.close()
    }
  }
}
