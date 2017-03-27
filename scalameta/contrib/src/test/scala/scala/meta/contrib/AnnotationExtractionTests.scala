package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta._

class AnnotationExtractionTests extends FunSuite {
  test("Test extract annotations from class") {
    val annots = q"@foo final class Foo".extract[Annotations]
    assert(annots.exists(_ isEqual mod"@foo"))
    assert(annots.size == 1)
  }
}
