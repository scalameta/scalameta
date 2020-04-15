package scala.meta.tests.tokenizers

import munit.FunSuite
import scala.meta.internal.tokenizers.ScalametaTokenizer
import scala.meta.Dialect
import scala.meta.inputs.Input.{File => MetaFile}
import scala.meta.io.AbsolutePath
import java.io.File
import scala.meta.internal.parsers.ScalametaParser

class Scala3TokensSuite extends FunSuite {

  val testDir = "/home/kpbochenek/vl/github/official/dotty-example-project/src/main/scala"
  
  test("first test".ignore) {
    val dialect = scala.meta.dialects.Dotty
    for (f <- new File(testDir).listFiles.filter(_.getName.endsWith(".scala"))) {
      println(s"Checking ${f.toPath.toString}")
      val result = ScalametaTokenizer.toTokenize(MetaFile(f), dialect).get
      val source = new ScalametaParser(MetaFile(f), dialect).parseSource()
    }
  }
}
