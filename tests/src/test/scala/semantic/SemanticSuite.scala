import org.scalatest._
import scala.meta._

class SemanticSuite extends FunSuite {
  import scala.meta.internal.hosts.scalac.Scalahost
  implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror)

  test("subtyping") {
    assert(t"List[Int]" <:< t"List[Any]")
  }
}