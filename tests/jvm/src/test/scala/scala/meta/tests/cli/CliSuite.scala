package scala.meta.tests.cli

import java.io._
import java.nio.charset.StandardCharsets._
import java.nio.file._
import org.scalatest.FunSuite
import scala.meta.cli._
import scala.meta.testkit.DiffAssertions

class CliSuite extends FunSuite with DiffAssertions {
  val sourceroot = Files.createTempDirectory("sourceroot_")
  val helloWorldScala = sourceroot.resolve("HelloWorld.scala")
  Files.write(helloWorldScala, """
    object HelloWorld {
      def main(args: Array[String]): Unit = {
        println("hello world")
      }
    }
  """.getBytes(UTF_8))
  val target = Files.createTempDirectory("target_")
  val helloWorldSemanticdb = target.resolve("META-INF/semanticdb/HelloWorld.semanticdb")

  private def communicate[T](op: => T): (T, String) = {
    val baos = new ByteArrayOutputStream
    val ps = new PrintStream(baos, true, UTF_8.name)
    val result = scala.Console.withOut(baos)(scala.Console.withErr(baos)(op))
    (result, new String(baos.toByteArray, UTF_8))
  }

  test("metac " + helloWorldScala) {
    val scalaLibraryJar = sys.props("sbt.paths.scalalibrary.classes")
    if (scalaLibraryJar == null) sys.error("sbt.paths.scalalibrary.classes not set. broken build?")
    val (exitcode, output) = communicate {
      Metac.process(Array(
        "-cp",
        scalaLibraryJar,
        "-P:semanticdb:sourceroot:" + sourceroot.toString,
        "-d",
        target.toString,
        helloWorldScala.toString))
    }
    assert(exitcode == 0)
    assert(output.isEmpty)
    assert(Files.exists(helloWorldSemanticdb))
  }

  test("metap " + helloWorldSemanticdb) {
    val (exitcode, output) = communicate {
      Metap.process(Array(helloWorldSemanticdb.toString))
    }
    assert(exitcode == 0)
    assertNoDiff(output, """
      |HelloWorld.scala
      |----------------
      |
      |Summary:
      |Format => semanticdb3
      |Uri => HelloWorld.scala
      |Text => non-empty
      |Language => Scala212
      |Symbols => 9 entries
      |Occurrences => 7 entries
      |Diagnostics => 0 entries
      |Synthetics => 0 entries
      |
      |Symbols:
      |_empty_.HelloWorld. => final object HelloWorld
      |_empty_.HelloWorld.main([Ljava/lang/String;)V. => def main: (args: Array[String]): Unit
      |  [0:7..0:12): Array => _root_.scala.Array#
      |  [0:13..0:19): String => _root_.scala.Predef.String#
      |  [0:23..0:27): Unit => _root_.scala.Unit#
      |_empty_.HelloWorld.main([Ljava/lang/String;)V.(args) => param args: Array[String]
      |  [0:0..0:5): Array => _root_.scala.Array#
      |  [0:6..0:12): String => _root_.scala.Predef.String#
      |_root_.scala.Array# => final class Array
      |_root_.scala.Array#`<init>`(I)V. => primaryctor <init>: (_length: Int): Array[T]
      |  [0:10..0:13): Int => _root_.scala.Int#
      |  [0:16..0:21): Array => _root_.scala.Array#
      |  [0:22..0:23): T => _root_.scala.Array#[T]
      |_root_.scala.Predef.String# => type String: String
      |  [0:0..0:6): String => _root_.java.lang.String#
      |_root_.scala.Predef.println(Ljava/lang/Object;)V. => def println: (x: Any): Unit
      |  [0:4..0:7): Any => _root_.scala.Any#
      |  [0:10..0:14): Unit => _root_.scala.Unit#
      |_root_.scala.Unit# => abstract final class Unit
      |_root_.scala.Unit#`<init>`()V. => primaryctor <init>: (): Unit
      |  [0:4..0:8): Unit => _root_.scala.Unit#
      |
      |Occurrences:
      |[1:11..1:21): HelloWorld <= _empty_.HelloWorld.
      |[2:10..2:14): main <= _empty_.HelloWorld.main([Ljava/lang/String;)V.
      |[2:15..2:19): args <= _empty_.HelloWorld.main([Ljava/lang/String;)V.(args)
      |[2:21..2:26): Array => _root_.scala.Array#
      |[2:27..2:33): String => _root_.scala.Predef.String#
      |[2:37..2:41): Unit => _root_.scala.Unit#
      |[3:8..3:15): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
    """.trim.stripMargin)
  }
}