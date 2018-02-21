package scala.meta.tests.metacp

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.mutable.ArrayBuffer
import scala.meta.internal.metacp.Javacp
import scala.meta.internal.metacp.Main
import scala.meta.internal.metacp.Settings
import scala.meta.tests.cli.BaseCliSuite
import scala.util.control.NonFatal
import org.langmeta.internal.io.PathIO
import org.langmeta.io.Classpath

abstract class BaseMetacpSuite extends BaseCliSuite {

  private val tmp = Files.createTempDirectory("metacp")
  tmp.toFile.deleteOnExit()

  def check(name: String, classpath: () => String): Unit = {
    test(name) {
      val cp = classpath()
      val args = Array[String](
        "-cp",
        cp,
        "-d",
        tmp.toString
      )
      println(tmp)
      val settings = Settings.parse(args.toList).get
      val exit = Main.process(settings)
      assert(exit == 0)
    }
  }

  def checkClassSignature(name: String, classpath: () => String): Unit = {
    test(name) {
      val failingSignatures = ArrayBuffer.empty[String]
      Classpath(classpath()).visit { root =>
        new java.nio.file.SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (PathIO.extension(file) == "class") {
              val bytes = Files.readAllBytes(file)
              val node = Javacp.asmNodeFromBytes(bytes)
              try {
                if (node.signature != null) {
                  ClassSignatureSuite.assertRoundtrip(node.signature)
                }
              } catch {
                case NonFatal(e) =>
                  println(node.signature)
                  failingSignatures += node.signature
              }
            }
            FileVisitResult.CONTINUE
          }
        }
      }
      Files.write(
        java.nio.file.Paths.get("signatures.txt"),
        failingSignatures.mkString("\n").getBytes(StandardCharsets.UTF_8)
      )
    }
  }

  case class Coordinates(organization: String, artifact: String, version: String) {
    def name = List(organization, artifact, version).mkString(File.pathSeparator)
    def classpath: () => String = { () =>
      val jars = Jars
        .fetch(organization, artifact, version)
        .filterNot(_.toString.contains("scala-lang"))
      jars.mkString(File.pathSeparator)
    }
  }

  val scalameta = Coordinates("org.scalameta", "scalameta_2.12", "3.2.0")
  val akka = Coordinates("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
  val spark = Coordinates("org.apache.spark", "spark-sql_2.11", "2.2.1")

  def checkLibrary(coordinates: Coordinates): Unit = {
    check(
      coordinates.name,
      coordinates.classpath
    )
  }

  def checkClassSignatureLibrary(coordinates: Coordinates): Unit = {
    checkClassSignature(
      coordinates.name,
      coordinates.classpath
    )
  }
}
