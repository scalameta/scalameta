package scala.meta.tests.metacp

import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.meta.internal.metacp.Javacp
import scala.tools.asm.tree.ClassNode
import scala.util.control.NonFatal
import org.langmeta.internal.io.PathIO
import org.langmeta.io.Classpath

abstract class BaseSignatureSuite extends BaseMetacpSuite {
  def checkSignatureCallback(name: String, classpath: () => String)(
      callback: ClassNode => List[(String, () => Unit)]
  ): Unit = {
    test(name) {
      val failingSignatures = ArrayBuffer.empty[String]
      Classpath(classpath()).visit { root =>
        new java.nio.file.SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (PathIO.extension(file) == "class") {
              val bytes = Files.readAllBytes(file)
              val node = Javacp.asmNodeFromBytes(bytes)
              val tests = callback(node)
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
}
