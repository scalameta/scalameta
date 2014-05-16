import org.scalatest.FunSuite

class BasicSuite extends FunSuite {
  test("hello world") {
    assert(Hello.world === "Hello world!")
  }
}