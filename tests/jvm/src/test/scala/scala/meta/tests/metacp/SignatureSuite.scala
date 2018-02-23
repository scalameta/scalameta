package scala.meta.tests.metacp

import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.metacp.Javacp
import scala.meta.internal.metacp.Scopes
import scala.meta.internal.metacp.Main
import scala.meta.internal.metacp.Settings
import scala.meta.internal.metacp.asm.ClassSignatureVisitor
import scala.meta.internal.metacp.asm.FieldSignatureVisitor
import scala.meta.internal.metacp.asm.JavaTypeSignature
import scala.meta.internal.metacp.asm.MethodSignatureVisitor
import scala.meta.internal.metacp.asm.JavaTypeSignature.Pretty
import scala.meta.internal.metacp.asm.TypedSignatureVisitor
import scala.tools.asm.signature.SignatureReader
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.FieldNode
import scala.tools.asm.tree.MethodNode
import scala.util.control.NonFatal
import scala.meta.internal.semanticdb3.TextDocuments
import scala.meta.internal.semanticdb3.SymbolInformation
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath
import org.langmeta.semanticdb.Database

class SignatureSuite extends BaseMetacpSuite {

  final def assertRoundtrip(signature: String, visitor: TypedSignatureVisitor[Pretty]): Unit = {
    val obtained = JavaTypeSignature.parse[Pretty](signature, visitor).pretty
    assertNoDiff(obtained, signature)
  }

  final def checkSignatureLibrary(coordinates: Coordinates): Unit = {
    test(coordinates.name) {
      var longestSignature = ""
      val failingSignatures = ArrayBuffer.empty[String]
      Classpath(coordinates.classpath()).visit { root =>
        new java.nio.file.SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (PathIO.extension(file) == "class") {
              val bytes = Files.readAllBytes(file)
              val node = Javacp.parseClassNode(bytes)
              val tests = callback(node)
              tests.foreach {
                case (signature, unsafe) =>
                  try {
                    unsafe()
                    if (signature.length > longestSignature.length)
                      longestSignature = signature
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
      println(longestSignature)
    }
  }

  def fieldCallback(node: ClassNode): List[(String, () => Unit)] =
    node.fields.asScala.iterator.map { field: FieldNode =>
      val signature = if (field.signature == null) field.desc else field.signature
      (signature, { () =>
        assertRoundtrip(signature, new FieldSignatureVisitor())
      })
    }.toList

  def methodCallback(node: ClassNode): List[(String, () => Unit)] =
    node.methods.asScala.iterator.map { method: MethodNode =>
      val signature = if (method.signature == null) method.desc else method.signature
      (signature, { () =>
        assertRoundtrip(signature, new MethodSignatureVisitor())
      })
    }.toList

  def classCallback(node: ClassNode): List[(String, () => Unit)] =
    if (node.signature == null) Nil
    else {
      List(
        (node.signature, { () =>
          assertRoundtrip(node.signature, new ClassSignatureVisitor)
        })
      )
    }

  def callback(node: ClassNode): List[(String, () => Unit)] = {
    fieldCallback(node) ::: methodCallback(node) ::: classCallback(node)
  }

  ignore("metacp") {
    val out = Files.createTempDirectory("metacp")
    out.toFile.deleteOnExit()

    Main.process(Settings("semanticdb/integration/target/scala-2.12/classes/" :: Nil, out.toString))
  }

  ignore("s.Type") {

    val path =
      AbsolutePath("semanticdb/integration/target/scala-2.12/classes/com/javacp/ClassSuffix.class")
    val bytes = path.readAllBytes
    val node = Javacp.parseClassNode(bytes)
    val scopes = new Scopes()
    val db = Javacp.ssymbols(node, scopes)
    db.foreach { s: SymbolInformation =>
      s.kind match {
        case k.TYPE_PARAMETER =>
        case k.VAR | k.VAL =>
        case _ =>
      }
    }
  }

  test("print") {
    val compiler = InteractiveSemanticdb.newCompiler()
    import scala.meta.internal.semanticdb._
    val doc = Database(
      InteractiveSemanticdb
        .toDocument(
          compiler,
          """
            |class Foo {
            |  def foo(e: Map[_ <: Number, _ >: String]): String = null
            |}
      """.stripMargin) :: Nil).toSchema(PathIO.workingDirectory)
    doc.documents.head.symbols.foreach { i: SymbolInformation =>
      if (i.kind == k.PARAMETER) {
        println(i.toProtoString)
      }
    }
  }

}
