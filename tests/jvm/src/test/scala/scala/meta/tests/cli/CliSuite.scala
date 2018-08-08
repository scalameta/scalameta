package scala.meta.tests.cli

import java.io._
import java.nio.charset.StandardCharsets._
import java.nio.file._
import org.scalatest.FunSuite
import scala.meta.cli._
import scala.meta.testkit._
import scala.meta.tests.metacp._

class CliSuite extends FunSuite with DiffAssertions {
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
        Library.scalaLibrary.classpath().syntax,
        "-P:semanticdb:sourceroot:" + sourceroot.toString,
        "-P:semanticdb:text:on",
        "-d",
        target.toString,
        helloWorldScala.toString)
      val settings = scala.meta.metac.Settings().withScalacArgs(scalacArgs)
      val reporter = Reporter()
      Metac.process(settings, reporter)
    }
    assert(success)
    assert(out.isEmpty)
    assert(Files.exists(helloWorldSemanticdb))
  }

  test("metap " + helloWorldSemanticdb) {
    val (success, out, err) = CliSuite.withReporter { reporter =>
      val format = scala.meta.metap.Format.Detailed
      val settings =
        scala.meta.metap.Settings().withFormat(format).withPaths(List(helloWorldSemanticdb))
      Metap.process(settings, reporter)
    }
    assert(success)
    assertNoDiffOrPrintExpected(
      out,
      """|HelloWorld.scala
       |----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => HelloWorld.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 3 entries
       |Occurrences => 7 entries
       |
       |Symbols:
       |_empty_/HelloWorld. => final object HelloWorld extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/HelloWorld.main(). => method main(args: Array[String]): Unit
       |  args => _empty_/HelloWorld.main().(args)
       |  Array => scala/Array#
       |  String => scala/Predef.String#
       |  Unit => scala/Unit#
       |_empty_/HelloWorld.main().(args) => param args: Array[String]
       |  Array => scala/Array#
       |  String => scala/Predef.String#
       |
       |Occurrences:
       |[1:11..1:21): HelloWorld <= _empty_/HelloWorld.
       |[2:10..2:14): main <= _empty_/HelloWorld.main().
       |[2:15..2:19): args <= _empty_/HelloWorld.main().(args)
       |[2:21..2:26): Array => scala/Array#
       |[2:27..2:33): String => scala/Predef.String#
       |[2:37..2:41): Unit => scala/Unit#
       |[3:8..3:15): println => scala/Predef.println(+1).
    """.trim.stripMargin)
    assert(err.isEmpty)
  }
}

object CliSuite {
  def withReporter[T](op: Reporter => T): (T, String, String) = {
    communicate { (out, err) =>
      op(Reporter().withOut(out).withErr(err))
    }
  }
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
