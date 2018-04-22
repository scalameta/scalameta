package scala.meta.tests.cli

import java.io._
import java.nio.charset.StandardCharsets._
import java.nio.file._
import scala.meta.cli._

class CliSuite extends BaseCliSuite {
  val sourceroot = Files.createTempDirectory("sourceroot_")
  val helloWorldScala = sourceroot.resolve("HelloWorld.scala")
  Files.write(
    helloWorldScala,
    """
    object HelloWorld {
      def main(args: Array[String]): Unit = {
        println("hello world")
      }
    }
  """.getBytes(UTF_8))
  val target = Files.createTempDirectory("target_")
  val helloWorldSemanticdb = target.resolve("META-INF/semanticdb/HelloWorld.scala.semanticdb")

  test("metac " + helloWorldScala) {
    val (success, out, err) = CliSuite.communicate { (out, err) =>
      val scalacArgs = List(
        "-cp",
        scalaLibraryJar,
        "-P:semanticdb:sourceroot:" + sourceroot.toString,
        "-d",
        target.toString,
        helloWorldScala.toString)
      val settings = scala.meta.metac.Settings().withScalacArgs(scalacArgs)
      val reporter = scala.meta.metac.Reporter()
      Metac.process(settings, reporter)
    }
    assert(success)
    assert(out.isEmpty)
    assert(Files.exists(helloWorldSemanticdb))
  }

  test("metap " + helloWorldSemanticdb) {
    val (success, out, err) = CliSuite.communicate { (out, err) =>
      val settings = scala.meta.metap.Settings().withPaths(List(helloWorldSemanticdb))
      val reporter = scala.meta.metap.Reporter().withOut(out).withErr(err)
      Metap.process(settings, reporter)
    }
    assert(success)
    assertNoDiff(
      out,
      """
      |HelloWorld.scala
      |----------------
      |
      |Summary:
      |Schema => SemanticDB v3
      |Uri => HelloWorld.scala
      |Text => non-empty
      |Language => Scala
      |Symbols => 3 entries
      |Occurrences => 7 entries
      |
      |Symbols:
      |_empty_.HelloWorld. => final object HelloWorld
      |_empty_.HelloWorld.main(Array). => method main: (args: Array[String]): Unit
      |  args => _empty_.HelloWorld.main(Array).(args)
      |  Array => scala.Array#
      |  String => scala.Predef.String#
      |  Unit => scala.Unit#
      |_empty_.HelloWorld.main(Array).(args) => param args: Array[String]
      |  Array => scala.Array#
      |  String => scala.Predef.String#
      |
      |Occurrences:
      |[1:11..1:21): HelloWorld <= _empty_.HelloWorld.
      |[2:10..2:14): main <= _empty_.HelloWorld.main(Array).
      |[2:15..2:19): args <= _empty_.HelloWorld.main(Array).(args)
      |[2:21..2:26): Array => scala.Array#
      |[2:27..2:33): String => scala.Predef.String#
      |[2:37..2:41): Unit => scala.Unit#
      |[3:8..3:15): println => scala.Predef.println(Any).
    """.trim.stripMargin)
    assert(err.isEmpty)
  }
}

object CliSuite {
  def communicate[T](op: (PrintStream, PrintStream) => T): (T, String, String) = {
    val outbaos = new ByteArrayOutputStream
    val outps = new PrintStream(outbaos, true, UTF_8.name)
    val errbaos = new ByteArrayOutputStream
    val errps = new PrintStream(errbaos, true, UTF_8.name)
    val result = op(outps, errps)
    val outs = new String(outbaos.toByteArray, UTF_8)
    val errs = new String(errbaos.toByteArray, UTF_8)
    (result, outs, errs)
  }
}
