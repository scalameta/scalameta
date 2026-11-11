package scala.meta.tests.semanticdb.dotc

import java.nio.file.{Files, Path}

class DotcStaleSuite extends munit.FunSuite {

  private val Range = "dotty/tools/dotc/semanticdb/Range.scala"

  test("generated output is current") {
    val diff = DotcOutput.diff(DotcRewriter(DotcOutput.Raw), DotcOutput.read(DotcOutput.Committed))
    assert(diff.isEmpty, diff.mkString("Run `sbt save-dotc`:\n", "\n", ""))
  }

  /**
   * Returns the lines of the diff between the committed output and a changed copy of it.
   *
   * @param edit
   *   a function that takes the directory of the copy and changes the copy
   */
  private def check(edit: Path => Unit): List[String] = {
    val committed = DotcOutput.read(DotcOutput.Committed)
    DotcOutput.withTempDir { copy =>
      DotcOutput.write(copy, committed)
      edit(copy)
      DotcOutput.diff(committed, DotcOutput.read(copy))
    }
  }

  test("one byte changed in a committed file") {
    val diff = check { dir =>
      val file = dir.resolve(Range)
      val bytes = Files.readAllBytes(file)
      bytes(0) = (bytes(0) ^ 1).toByte
      Files.write(file, bytes)
    }
    assertEquals(diff, List(s"$Range differs"))
  }

  test("a committed file deleted")(
    assertEquals(check(dir => Files.delete(dir.resolve(Range))), List(s"$Range is missing")),
  )

  test("a committed file that the generation does not write") {
    val diff = check(dir => Files.write(dir.resolve("Extra.scala"), Array.emptyByteArray))
    assertEquals(diff, List("Extra.scala is not generated"))
  }

}
