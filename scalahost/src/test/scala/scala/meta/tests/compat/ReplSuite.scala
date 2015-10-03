package scala.meta.tests
package compat

import org.scalatest._
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._

class ReplSuite extends FunSuite {
  private def repl(code: String): String = {
    val s = new Settings
    s.Xnojline.value = true
    s.usejavacp.value = false
    s.classpath.value = sys.props("sbt.paths.scalahost.classes")
    val lines = ILoop.runForTranscript(code, s).lines.toList
    lines.drop(3).map(_.replaceAll("\\s+$","")).mkString("\n").trim.stripSuffix("scala>").trim
  }

  test("semantic APIs") {
    assert(repl("""
      |import scala.meta._
      |import scala.meta.dialects.Scala211
      |implicit val c = Context(Artifact(sys.props("sbt.paths.scalalibrary.classes")))
      |t"List[Int]" <:< t"List[Any]"
    """.stripMargin.trim)
    .replace(sys.props("sbt.paths.scalalibrary.classes"), "<path/to/scala-library.jar>")
    === """
      |scala> import scala.meta._
      |import scala.meta._
      |
      |scala> import scala.meta.dialects.Scala211
      |import scala.meta.dialects.Scala211
      |
      |scala> implicit val c = Context(Artifact(sys.props("sbt.paths.scalalibrary.classes")))
      |c: scala.meta.Context = Context(Artifact("<path/to/scala-library.jar>", "", Scala211))
      |
      |scala> t"List[Int]" <:< t"List[Any]"
      |res0: Boolean = true
      |
      |scala> :quit
    """.stripMargin.trim)
  }
}