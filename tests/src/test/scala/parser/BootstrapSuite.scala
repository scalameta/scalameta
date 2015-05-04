import org.scalatest._
import java.net._
import java.io._

class BootstrapSuite extends ParseSuite {
  var dir = new File(new File(System.getProperty("sbt.paths.tests.source")).getAbsolutePath)
  def isProjectRoot(dir: File) = dir != null && new File(dir.getAbsolutePath + File.separatorChar + "project" + File.separatorChar + "build.scala").exists
  while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
  test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))

  if (isProjectRoot(dir)) {
    def loop(dir: File): Unit = {
      def bootstrapTest(src: File): Unit = {
        test("tokenize " + src.getAbsolutePath) {
          import scala.meta._
          import scala.meta.dialects.Scala211
          val toks = src.tokens
          val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
          val content = scala.io.Source.fromFile(src)(codec).mkString
          // check #1: everything's covered
          var isFail = false
          def fail(msg: String) = { isFail = true; println(msg) }
          val bitmap = new Array[Boolean](content.length)
          val tokenmap = scala.collection.mutable.Map[Int, List[Token]]()
          toks.foreach(tok => {
            var i = tok.start
            while (i <= tok.end) {
              if (i < 0 || content.length <= i) fail("TOKEN OUT OF BOUNDS AT " + i + ": " + tok)
              else {
                tokenmap(i) = tok +: tokenmap.getOrElse(i, Nil)
                if (bitmap(i)) fail("TOKENS OVERLAP AT " + i + ": " + tokenmap(i).mkString(", "))
                bitmap(i) = true
              }
              i += 1
            }
          })
          bitmap.zipWithIndex.filter(!_._1).foreach{ case (_, i) => fail("TOKENS DON'T COVER " + i) }
          // check #2: tostring works
          if (!isFail && content != toks.map(_.show[Code]).mkString) {
            isFail = true
            println("CORRELATION FAILED")
            println("EXPECTED: \n" + content)
            println("ACTUAL: \n" + toks.map(_.show[Code]).mkString)
          }
          assert(!isFail)
        }
        test("parse " + src.getAbsolutePath) {
          try {
            import scala.meta._
            import scala.meta.dialects.Scala211
            val tree = src.parse[Source]
            // check #1: everything's positioned
            def check(tree: Tree): Boolean = {
              def loop(x: Any): Boolean = x match {
                case x: Tree => check(x)
                case x: ::[_] => x.forall(loop)
                case x: Some[_] => loop(x.get)
                case x => true
              }
              tree.origin match {
                case _: Origin.Parsed => tree.productIterator.toList.forall(loop)
                case _ => false
              }
            }
            if (!check(tree)) {
              import scala.meta.ui.Positions.Colorful
              println(tree.show[Positions])
              assert(false)
            }
            // check #2: everything's covered
            val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
            val content = scala.io.Source.fromFile(src)(codec).mkString
            assert(tree.origin.start.offset == 0)
            assert(tree.origin.end.offset == content.length - 1)
            assert(tree.origin.start.line == 0)
            assert(tree.origin.end.line == content.count(_ == '\n'))
          } catch {
            case ex: scala.meta.ParseException if ex.message.contains("XML literals are not supported") => pending
          }
        }
      }
      dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).map(bootstrapTest)
      dir.listFiles.filter(_.isDirectory).map(loop)
    }
    loop(dir)
  }
}