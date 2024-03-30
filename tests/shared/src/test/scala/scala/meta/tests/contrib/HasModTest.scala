package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

import munit.FunSuite

class HasModTest extends FunSuite {

  test("Test hasMod(mod)") {
    assert(q"@foo final class Foo".hasMod(mod"final"))
    assert(!q"@foo class Foo".hasMod(mod"final"))
    assert(q"lazy val x = 2".hasMod(mod"lazy"))
    assert(!q"val x = 2".hasMod(mod"lazy"))
  }
}
