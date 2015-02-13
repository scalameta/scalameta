import org.scalatest._
import scala.meta._

class SemanticSuite extends FunSuite {
  import scala.meta.internal.hosts.scalac.Scalahost
  private val classpathOptions = s"-cp ${sys.props("sbt.paths.scala-library.jar")}"
  private val pluginOptions = s"-Xplugin:${sys.props("sbt.paths.plugin.jar")} -Xplugin-require:scalahost"
  private val options = classpathOptions + " " + pluginOptions
  implicit val c = Scalahost.mkEasyContext(options)

  test("subtyping") {
    assert(t"List[Int]" <:< t"List[Any]")
  }
}