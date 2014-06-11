import org.scalatest._

class OldSuite extends FunSuite {
  test("old join") {
    import scala.language.reflectiveCalls
    val result = Join(new { val x = 2 }, new { val y = 3 })
    assert(result.x === 2)
    assert(result.y === 3)
  }
}