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
    s.classpath.value = sys.props("sbt.paths.tests.classes")
    val lines = ILoop.runForTranscript(code, s).lines.toList
    lines.drop(3).map(_.replaceAll("\\s+$","")).mkString("\n").trim.stripSuffix("scala>").trim
  }

  test("semantic APIs") {
    assert(repl("""
      |import scala.meta._
      |import scala.meta.dialects.Scala211
      |implicit val mirror = Mirror(Artifact(sys.props("sbt.paths.scalalibrary.classes")))
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
      |scala> implicit val mirror = Mirror(Artifact(sys.props("sbt.paths.scalalibrary.classes")))
      |mirror: scala.meta.Mirror = Mirror(Artifact("<path/to/scala-library.jar>", "", Scala211))
      |
      |scala> t"List[Int]" <:< t"List[Any]"
      |scala.NotImplementedError: an implementation is missing
      |  at scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
      |  at scala.meta.internal.hosts.scalac.contexts.Proxy.typecheck(Proxy.scala:54)
      |  at scala.meta.internal.hosts.scalac.contexts.Proxy.isSubType(Proxy.scala:85)
      |  at scala.meta.semantic.Api$XtensionSemanticType.$less$colon$less(Api.scala:166)
      |  ... 103 elided
      |
      |scala> :quit
    """.stripMargin.trim)
  }
}