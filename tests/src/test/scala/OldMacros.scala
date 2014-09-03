import org.scalatest._

class OldMacros extends FunSuite {
  test("old join") {
    import scala.language.reflectiveCalls
    val x = new { val x = 2 }
    val y = new { val y = 3 }
    val result = Join(x, y)
    assert(result.x === 2)
    assert(result.y === 3)
  }
}