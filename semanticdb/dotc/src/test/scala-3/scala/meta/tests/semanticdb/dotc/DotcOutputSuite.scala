package scala.meta.tests.semanticdb.dotc

import scala.meta._

import java.nio.file.Files

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters._
import scala.util.Using

class DotcOutputSuite extends munit.FunSuite {

  /** Returns each annotation in `files` for which `DotcRewriter.kept` is false, with its path. */
  private def unkept(files: SortedMap[String, String]): List[String] = files.toList
    .flatMap { case (path, text) =>
      dialects.Scala3(Input.VirtualFile(path, text)).parse[Source].get.collect {
        case t: Mod.Annot if !DotcRewriter.kept(t) => s"$path: ${t.pos.text}"
      }
    }

  test("annotations of the committed output")(
    assertEquals(unkept(DotcOutput.read(DotcOutput.Committed)), Nil),
  )

  test("annotations of a message") {
    val raw =
      """|package dotty.tools.dotc.semanticdb
         |
         |@SerialVersionUID(0L) @_root_.scala.deprecated
         |@deprecated
         |final case class Range(
         |    @scala.deprecated(message = "x", "") startLine: _root_.scala.Int = 0,
         |    endLine: _root_.scala.Int = (0: @unchecked)
         |    ) extends scalapb.GeneratedMessage {
         |    @transient
         |    private var __serializedSizeMemoized: _root_.scala.Int = 0
         |    @java.lang.Deprecated def withEndLine(__v: Int): Range = copy(endLine = __v)
         |}
         |""".stripMargin
    val rewritten =
      """|package dotty.tools.dotc.semanticdb
         |import dotty.tools.dotc.semanticdb.internal.*
         |import scala.annotation.internal.sharable
         |
         |@SerialVersionUID(0L)
         |final case class Range(
         |    startLine: _root_.scala.Int = 0,
         |    endLine: _root_.scala.Int = (0: @unchecked)
         |    ) extends SemanticdbGeneratedMessage derives CanEqual {
         |    @transient @sharable
         |    private var __serializedSizeMemoized: _root_.scala.Int = 0
         |    def withEndLine(__v: Int): Range = copy(endLine = __v)
         |}
         |""".stripMargin
    val output = DotcOutput.withTempDir { dir =>
      DotcOutput.write(dir, SortedMap("Range.scala" -> raw))
      DotcRewriter(dir)
    }
    assertEquals(output, SortedMap("Range.scala" -> rewritten))
    assertEquals(unkept(output), List("Range.scala: @unchecked"))
  }

  test("every class file is in the package of the messages or of the runtime") {
    val packages = Set("dotty/tools/dotc/semanticdb", "dotty/tools/dotc/semanticdb/internal")
    val dir = DotcOutput.Classes
    val files = Using.resource(Files.walk(dir))(_.iterator.asScala.toList)
      .filter(_.toString.endsWith(".class"))
    assert(files.nonEmpty, s"no class file in $dir")
    val outside = files
      .filterNot(file => packages(dir.relativize(file.getParent).iterator.asScala.mkString("/")))
    assertEquals(outside, Nil)
  }

}
