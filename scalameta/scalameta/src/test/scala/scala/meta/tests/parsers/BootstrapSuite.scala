package scala.meta.tests
package parsers

import org.scalatest._
import java.net._
import java.io._
import scala.meta._
import scala.meta.tokens.Token._
import scala.meta.internal.prettyprinters.Positions
import scala.meta.dialects.Scala211

class BootstrapSuite extends ParseSuite {
  var dir = new File(new File(System.getProperty("sbt.paths.scalameta.test.sources")).getAbsolutePath)
  def isProjectRoot(dir: File) = dir != null && new File(dir.getAbsolutePath + File.separatorChar + "project" + File.separatorChar + "build.scala").exists
  while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
  test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))

  if (isProjectRoot(dir)) {
    def loop(dir: File): Unit = {
      def bootstrapTest(src: File): Unit = {
        test("tokenize " + src.getAbsolutePath) {
          val tokens = src.tokenize.get
          val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
          val content = scala.io.Source.fromFile(src)(codec).mkString
          // check #1: everything's covered
          var isFail = false
          def fail(msg: String) = { isFail = true; println(msg) }
          val bitmap = new Array[Boolean](content.length)
          val tokenmap = scala.collection.mutable.Map[Int, List[Token]]()
          tokens.foreach(tok => {
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
          })
          bitmap.zipWithIndex.filter(!_._1).foreach{ case (_, i) => fail("TOKENS DON'T COVER " + i) }
          // check #2: syntax works
          if (!isFail && content != tokens.syntax.mkString) {
            isFail = true
            println("CORRELATION FAILED")
            println("EXPECTED: \n" + content)
            println("ACTUAL: \n" + tokens.syntax.mkString)
          }
          assert(!isFail)
        }
        // TODO: fix performance, see #382
        // test("parse " + src.getAbsolutePath) {
        //   val tree = src.parse[Source].get
        //   // check #1: everything's positioned
        //   tree.traverse {
        //     case sub =>
        //       if (sub.pos.isEmpty) {
        //         import scala.meta.internal.prettyprinters.PositionStyle.Colorful
        //         println(tree.show[Positions])
        //         fail()
        //       }
        //       if (sub.tokens.nonEmpty) {
        //         assert(!sub.tokens.head.is[BOF])
        //         assert(!sub.tokens.last.is[EOF])
        //         assert(sub.pos.start.offset == sub.tokens.head.start)
        //         assert(sub.pos.end.offset == sub.tokens.last.end)
        //       }
        //   }
        //   // check #2: everything's covered
        //   val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
        //   val content = scala.io.Source.fromFile(src)(codec).mkString
        //   assert(tree.pos.start.offset == 0)
        //   assert(tree.pos.end.offset == content.length)
        //   assert(tree.pos.start.line == 0)
        //   assert(tree.pos.end.line == content.count(_ == '\n'))
        // }
      }
      dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).map(bootstrapTest)
      dir.listFiles.filter(_.isDirectory).map(loop)
    }
    loop(dir)
  }
}