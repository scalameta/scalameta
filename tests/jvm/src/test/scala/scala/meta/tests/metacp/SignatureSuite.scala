package scala.meta.tests.metacp

import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import org.scalatest.FunSuite
import org.scalatest.Ignore
import org.scalatest.tagobjects.Slow
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.meta.internal.io._
import scala.meta.internal.javacp._
import scala.meta.internal.javacp.asm._
import scala.meta.internal.metacp._
import scala.meta.io.AbsolutePath
import scala.meta.testkit._
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.FieldNode
import scala.tools.asm.tree.MethodNode
import scala.util.control.NonFatal

class SignatureSuite extends FunSuite with DiffAssertions {

  // Validates that pretty(parse(signature) == signature
  def assertSignatureRoundtrip(
      signature: String,
      visitor: TypedSignatureVisitor[Printable]): Unit = {
    val obtained = JavaTypeSignature.parse[Printable](signature, visitor).pretty
    assertNoDiff(obtained, signature)
  }

  // Validates that all signatures of the classfiles in the given
  // library pass assertSignatureRoundtrip
  def checkSignatureRoundtrip(library: Library): Unit = {
    test(library.name, Slow) {
      val failingSignatures = ArrayBuffer.empty[String]
      library.classpath().visit { root =>
        new java.nio.file.SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (PathIO.extension(file) == "class") {
              val node = AbsolutePath(file).toClassNode
              val tests = checkAllSignatures(node)
              tests.foreach {
                case (signature, unsafe) =>
                  try {
                    unsafe()
                  } catch {
                    case NonFatal(e) =>
                      println(signature)
                      failingSignatures += signature
                  }
              }
            }
            FileVisitResult.CONTINUE
          }
        }
      }

      if (failingSignatures.nonEmpty) {
        Files.write(
          java.nio.file.Paths.get("signatures.txt"),
          failingSignatures.mkString("\n").getBytes(StandardCharsets.UTF_8)
        )
        fail("failures! See signatures.txt")
      }
    }
  }

  def checkFields(node: ClassNode): List[(String, () => Unit)] =
    node.fields.asScala.map { field: FieldNode =>
      val signature = if (field.signature == null) field.desc else field.signature
      (signature, { () =>
        assertSignatureRoundtrip(signature, new FieldSignatureVisitor())
      })
    }.toList

  def checkMethods(node: ClassNode): List[(String, () => Unit)] =
    node.methods.asScala.map { method: MethodNode =>
      val signature = if (method.signature == null) method.desc else method.signature
      (signature, { () =>
        assertSignatureRoundtrip(signature, new MethodSignatureVisitor())
      })
    }.toList

  def checkClass(node: ClassNode): List[(String, () => Unit)] =
    if (node.signature == null) Nil
    else {
      List(
        (node.signature, { () =>
          assertSignatureRoundtrip(node.signature, new ClassSignatureVisitor)
        })
      )
    }

  def checkAllSignatures(node: ClassNode): List[(String, () => Unit)] = {
    checkFields(node) ::: checkMethods(node) ::: checkClass(node)
  }

  Libraries.suite.foreach(checkSignatureRoundtrip)

}
