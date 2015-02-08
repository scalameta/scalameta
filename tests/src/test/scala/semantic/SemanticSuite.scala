import org.scalatest._
import scala.meta._

class SemanticSuite extends FunSuite {
  import scala.meta.internal.hosts.scalac.Scalahost
  private val options = s"-Xplugin:${sys.props("sbt.paths.plugin.jar")} -Xplugin-require:scalahost"
  implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror, options)

  test("subtyping") {
    assert(t"List[Int]" <:< t"List[Any]")
  }
}