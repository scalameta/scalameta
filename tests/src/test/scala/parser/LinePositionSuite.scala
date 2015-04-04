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
    case clss: impl.Defn.Class =>
      clss
    case df: impl.Defn.Def =>
      df
    case mtch: impl.Term.Match =>
      mtch
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
            case clss: impl.Defn.Class =>
              val startLine = lines(clss.origin.startLine)
              val endLine = lines(clss.origin.endLine)
              assert(startLine.contains("class " + clss.name.toString))
              assert(clss.origin.startLine <= clss.origin.endLine)
              assert(endLine.endsWith(")") || endLine.trim.endsWith("}") || endLine.endsWith("]") || endLine.contains("extends"))
            case df: impl.Defn.Def =>
              val startLine = lines(df.origin.startLine)
              assert(startLine.contains("def " + df.name.toString))
              assert(df.origin.startLine <= df.origin.endLine)
            case mtch: impl.Term.Match =>
              val startLine = lines(mtch.origin.startLine)
              val endLine = lines(mtch.origin.endLine)
              assert(mtch.origin.startLine <= mtch.origin.endLine)
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
