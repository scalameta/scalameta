import org.scalatest._
import java.net._
import java.io._

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.{ast => impl}
import scala.meta.tql._

import scala.util.{Try, Failure, Success}

class InferAndReparseSuite extends ParseSuite {
  var dir = new File(new File(System.getProperty("sbt.paths.tests.source")).getAbsolutePath)
  def isProjectRoot(dir: File) = dir != null && new File(dir.getAbsolutePath + File.separatorChar + "project" + File.separatorChar + "build.scala").exists
  while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
  test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))

  val ignoredFiles = List("build.scala")

  val transformAll = topDown(transform {
      case t: impl.Lit.Bool => t.copy() andCollect Unit
      case t: impl.Lit.Byte => t.copy() andCollect Unit
      case t: impl.Lit.Short => t.copy() andCollect Unit
      case t: impl.Lit.Int => t.copy() andCollect Unit
      case t: impl.Lit.Long => t.copy() andCollect Unit
      case t: impl.Lit.Float => t.copy() andCollect Unit
      case t: impl.Lit.Double => t.copy() andCollect Unit
      case t: impl.Lit.Char => t.copy() andCollect Unit
      case t: impl.Lit.String => t.copy() andCollect Unit
      case t: impl.Lit.Symbol => t.copy() andCollect Unit
      case t: impl.Lit.Null => t.copy() andCollect Unit
      case t: impl.Lit.Unit => t andCollect Unit
      case t: impl.Defn.Class => t.copy() andCollect Unit 
      case t: impl.Defn.Def => t.copy() andCollect Unit
      case t: impl.Defn.Trait => t.copy() andCollect Unit
      //case t: impl.Term.Match => t.copy() andCollect Unit
    // TODO: add more cases
  })

  def loop(dir: File): Unit = {
    def linePositionTest(src: File): Unit = {
      test("Testing synthetic tokens for file " + src.getAbsolutePath) {
        assert(src.exists)
        val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
        val content = scala.io.Source.fromFile(src)(codec).mkString
        val parsed = src.parse[Source]
        val transformed = transformAll(parsed)
        val newCode = transformed.tree.get.tokens.map(_.show[Code]).mkString
        Try(newCode.parse[Source]) match {
          case Success(_) => /* cool */
          case Failure(err) =>
            println("====================================================")
            println(content)
            println("----------------------------------------------------")
            println(newCode)
            println("----------------------------------------------------")
            println(err.getMessage)
            println("====================================================")
            fail
        }
      }
    }
    dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).filter(f => !ignoredFiles.contains(f.getName)).map(linePositionTest(_))
    dir.listFiles.filter(_.isDirectory).map(loop(_))
  }
  loop(dir)
}
