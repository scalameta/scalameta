package scala.meta.internal

import org.scalameta.collections.Conversions._
import scala.meta.internal.classpath._
import scala.meta.internal.scalacp.{ScalaSigAttribute, ScalaSigNode}
import scala.meta.io.AbsolutePath

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util

import scala.reflect.internal.pickling.ByteCodecs
import scala.tools.asm.ClassReader._
import scala.tools.asm._
import scala.tools.asm.tree._
import scala.tools.scalap.Main
import scala.tools.scalap.scalax.rules.scalasig.{ByteCode, ScalaSig, ScalaSigAttributeParsers}

package object metacp {
  implicit class XtensionClassNode(private val node: ClassNode) extends AnyVal {
    def scalaSig: Option[ScalaSigNode] =
      if (node.attrs == null) None
      else for {
        scalaSigAttribute <- node.attrs.toScala.collectFirst { case ScalaSigAttribute(scalaSig) =>
          scalaSig
        }
        scalaSig <- {
          if (scalaSigAttribute.table.nonEmpty) Some(scalaSigAttribute) else fromScalaSigAnnotation
        }
      } yield ScalaSigNode(node.name + ".class", scalaSig)
    private def fromScalaSigAnnotation: Option[ScalaSig] = Option(node.visibleAnnotations).flatMap {
      _.toScala.iterator.filter(annot =>
        annot.desc == Main.SCALA_SIG_ANNOTATION || annot.desc == Main.SCALA_LONG_SIG_ANNOTATION,
      ).map(_.values.toScala).collectFirst { case collection.Seq("bytes", anyBytes) =>
        val baos = new ByteArrayOutputStream()
        val bytes: Array[Byte] = anyBytes match {
          case bytesString: String => bytesString.getBytes(StandardCharsets.UTF_8)
          case bytesArray: util.ArrayList[_] =>
            bytesArray.toScala.foreach { case bytesString: String =>
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
  implicit class XtensionAsmPathOps(private val path: AbsolutePath) extends AnyVal {
    def toClassNode: ClassNode =
      readInputStreamToClassNode(Files.newInputStream(path.toNIO), path.toString)
  }

  implicit class XtensionAsmClassfileOps(private val classfile: Classfile) extends AnyVal {
    def toClassNode: ClassNode = readInputStreamToClassNode(classfile.openInputStream(), location)
    def hasScalaSig: Boolean = {
      val classNode = readInputStreamToClassNode(classfile.openInputStream(), location)
      classNode.attrs != null && classNode.attrs.toScala.exists(_.`type` match {
        case "Scala" | "ScalaSig" => true
        case _ => false
      })
    }
    private def location: String = classfile match {
      case UncompressedClassfile(_, path) => path.toString
      case CompressedClassfile(entry, zip) => s"$zip!/${entry.getName}"
    }
  }

  private def readInputStreamToClassNode(in: InputStream, location: String): ClassNode = {
    val node = new ClassNode()
    try {
      val reader =
        try new ClassReader(in)
        catch {
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException(s"${e.getMessage} in $location", e)
        }
      reader.accept(
        node,
        Array(ScalaSigAttribute),
        // NOTE(olafur): don't use SKIP_DEBUG field since it strips away Java
        // method parameter names since 2.12.11.
        SKIP_CODE | SKIP_FRAMES,
      )
      node
    } finally in.close()
  }
}
