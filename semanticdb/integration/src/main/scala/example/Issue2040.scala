// See https://github.com/scalameta/scalameta/issues/2040
package example

import scala.language.implicitConversions

object Issue2040 {
  trait Prettifier
  object Prettifier {
    implicit val default: Prettifier = ???
  }

  trait AnyShouldWrapper {
    def shouldBe(right: Any): Boolean
  }

  trait Base {
    def i() = 1
  }

  trait FooSpec extends Base {
    implicit def convertToAnyShouldWrapper(o: Any): AnyShouldWrapper

    i shouldBe 1
  }

  trait BarSpec extends Base {
    implicit def convertToAnyShouldWrapper(o: Any)(
        implicit prettifier: Prettifier
    ): AnyShouldWrapper

    i shouldBe 1
  }
}
