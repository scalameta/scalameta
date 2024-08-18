package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.tokens.Token._

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class BootstrapSuite extends ParseSuite {
  var dir = new File(new File(sys.props("sbt.paths.tests.test.sources")).getAbsolutePath)
  def isProjectRoot(dir: File) = dir != null &&
    new File(dir.getAbsolutePath + File.separatorChar + "build.sbt").exists
  while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
  test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))

  if (isProjectRoot(dir)) {
    def loop(dir: File): Unit = {
      val name = dir.getName
      if (dir.isDirectory && name == "target" || name == "community-projects") return
      def bootstrapTest(src: File): Unit = test("tokenize " + src.getAbsolutePath) {
        val tokens = src.tokenize.get
        val content = new String(Files.readAllBytes(src.toPath), StandardCharsets.UTF_8)
        // check #1: everything's covered
        var isFail = false
        def fail(msg: String) = { isFail = true; println(msg) }
        val bitmap = new Array[Boolean](content.length)
        val tokenmap = scala.collection.mutable.Map[Int, List[Token]]()
        tokens.foreach { tok =>
          var i = tok.start
          while (i < tok.end) {
            if (i < 0 || content.length <= i) fail("TOKEN OUT OF BOUNDS AT " + i + ": " + tok)
            else {
              tokenmap(i) = tok +: tokenmap.getOrElse(i, Nil)
              if (bitmap(i)) fail("TOKENS OVERLAP AT " + i + ": " + tokenmap(i).mkString(", "))
              bitmap(i) = true
            }
            i += 1
          }
        }
        bitmap.zipWithIndex.filter(!_._1).foreach { case (_, i) => fail("TOKENS DON'T COVER " + i) }
        // check #2: syntax works
        if (!isFail && content != tokens.syntax.mkString) {
          isFail = true
          println("CORRELATION FAILED")
          println("EXPECTED: \n" + content)
          println("ACTUAL: \n" + tokens.syntax.mkString)
        }
        assert(!isFail)
      }
      dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).foreach(bootstrapTest)
      dir.listFiles.filter(_.isDirectory).foreach(loop)
    }
    loop(dir)
  }
}
