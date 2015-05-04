import org.scalatest._
import java.net._
import java.io._

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.{ast => impl}
import scala.meta.tql._

class LinePositionSuite extends ParseSuite {
  var dir = new File(new File(System.getProperty("sbt.paths.tests.source")).getAbsolutePath)
  def isProjectRoot(dir: File) = dir != null && new File(dir.getAbsolutePath + File.separatorChar + "project" + File.separatorChar + "build.scala").exists
  while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
  test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))

  val ignoredFiles = List(
    "build.scala",
    "Token.scala",
    "Api.scala"
  )

  val findAllDefn = topDown(collect {
    case x: impl.Defn.Class => x
    case x: impl.Defn.Def => x
    case x: impl.Term.Match => x
  })

  def loop(dir: File): Unit = {
    def linePositionTest(src: File): Unit = {
      test("Testing line pos for " + src.getAbsolutePath) {
        assert(src.exists)
        val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
        val content = scala.io.Source.fromFile(src)(codec).mkString
        val tree = content.parse[Source]
        val lines = content.lines.toList
        val matches: List[impl.Tree] = findAllDefn(tree)
        matches foreach { mtch =>
          mtch match {
            case x: impl.Defn.Class =>
              val startLine = lines(x.origin.start.line)
              val endLine = lines(x.origin.end.line)
              assert(startLine.contains("class " + x.name.toString))
              assert(x.origin.start.line <= x.origin.end.line)
              assert(endLine.endsWith(")") || endLine.trim.endsWith("}") || endLine.endsWith("]") || endLine.contains("extends"))
            case x: impl.Defn.Def =>
              val startLine = lines(x.origin.start.line)
              assert(startLine.contains("def " + x.name.toString))
              assert(x.origin.start.line <= x.origin.end.line)
            case x: impl.Term.Match =>
              val startLine = lines(x.origin.start.line)
              val endLine = lines(x.origin.end.line)
              assert(x.origin.start.line <= x.origin.end.line)
              assert(startLine.contains("match"))
              assert(endLine.contains("}"))
            case _ => assert(false)

          }
        }
      }
    }
    dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).filter(f => !ignoredFiles.contains(f.getName)).map(linePositionTest(_))
    dir.listFiles.filter(_.isDirectory).map(loop(_))
  }
  loop(dir)
}
