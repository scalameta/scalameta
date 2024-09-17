package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

import munit.FunSuite

class AnnotationExtractionTests extends FunSuite {
  test("Test extract annotations from class") {
    val annots = q"@foo final class Foo".extract[Mod.Annot]
    assert(annots.exists(_.isEqual(mod"@foo")))
    assertEquals(annots.size, 1)
  }
}
