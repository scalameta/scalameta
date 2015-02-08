import org.scalatest._
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._

class ReplSuite extends FunSuite {
  private def repl(code: String): String = {
    val s = new Settings
    s.Xnojline.value = true
    s.usejavacp.value = false
    s.classpath.value = sys.props("sbt.paths.tests.classpath")
    s.plugin.value = List(sys.props("sbt.paths.plugin.jar"))
    val lines = ILoop.runForTranscript(code, s).lines.toList
    lines.drop(3).map(_.replaceAll("\\s+$","")).mkString("\n").trim.stripSuffix("scala>").trim
  }

  test("semantic APIs") {
    assert(repl("""
      |import scala.meta._
      |import scala.meta.internal.hosts.scalac.Scalahost
      |val options = "-Xplugin:" + sys.props("sbt.paths.plugin.jar") + " -Xplugin-require:scalahost"
      |implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror, options)
      |t"List[Int]" <:< t"List[Any]"
    """.stripMargin.trim).replace(sys.props("sbt.paths.plugin.jar"), "<path/to/plugin.jar>").replaceAll("ToolboxContext@[0-9a-fA-F]+", "ToolboxContext@<memoryAddress>") === """
      |scala> import scala.meta._
      |import scala.meta._
      |
      |scala> import scala.meta.internal.hosts.scalac.Scalahost
      |import scala.meta.internal.hosts.scalac.Scalahost
      |
      |scala> val options = "-Xplugin:" + sys.props("sbt.paths.plugin.jar") + " -Xplugin-require:scalahost"
      |options: String = -Xplugin:<path/to/plugin.jar> -Xplugin-require:scalahost
      |
      |scala> implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror, options)
      |c: scala.meta.internal.hosts.scalac.ToolboxContext = scala.meta.internal.hosts.scalac.ToolboxContext@<memoryAddress>
      |
      |scala> t"List[Int]" <:< t"List[Any]"
      |res0: Boolean = true
      |
      |scala> :quit
    """.stripMargin.trim)
  }
}