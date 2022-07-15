// See https://github.com/scalameta/scalameta/issues/2040
package example

import scala.language/*=>scala.language.*/.implicitConversions/*=>scala.language.implicitConversions.*/

object Issue2040/*<=example.Issue2040.*/ {
  trait Prettifier/*<=example.Issue2040.Prettifier#*/
  object Prettifier/*<=example.Issue2040.Prettifier.*/ {
    implicit val default/*<=example.Issue2040.Prettifier.default.*/: Prettifier/*=>example.Issue2040.Prettifier#*/ = ???/*=>scala.Predef.`???`().*/
  }

  trait AnyShouldWrapper/*<=example.Issue2040.AnyShouldWrapper#*/ {
    def shouldBe/*<=example.Issue2040.AnyShouldWrapper#shouldBe().*/(right/*<=example.Issue2040.AnyShouldWrapper#shouldBe().(right)*/: Any/*=>scala.Any#*/): Boolean/*=>scala.Boolean#*/
  }

  trait Base/*<=example.Issue2040.Base#*/ {
    def i/*<=example.Issue2040.Base#i().*/() = 1
  }

  trait FooSpec/*<=example.Issue2040.FooSpec#*/ extends Base/*=>example.Issue2040.Base#*/ {
    implicit def convertToAnyShouldWrapper/*<=example.Issue2040.FooSpec#convertToAnyShouldWrapper().*/(o/*<=example.Issue2040.FooSpec#convertToAnyShouldWrapper().(o)*/: Any/*=>scala.Any#*/): AnyShouldWrapper/*=>example.Issue2040.AnyShouldWrapper#*/

    i/*=>example.Issue2040.Base#i().*/ shouldBe/*=>example.Issue2040.AnyShouldWrapper#shouldBe().*/ 1
  }

  trait BarSpec/*<=example.Issue2040.BarSpec#*/ extends Base/*=>example.Issue2040.Base#*/ {
    implicit def convertToAnyShouldWrapper/*<=example.Issue2040.BarSpec#convertToAnyShouldWrapper().*/(o/*<=example.Issue2040.BarSpec#convertToAnyShouldWrapper().(o)*/: Any/*=>scala.Any#*/)(
        implicit prettifier/*<=example.Issue2040.BarSpec#convertToAnyShouldWrapper().(prettifier)*/: Prettifier/*=>example.Issue2040.Prettifier#*/
    ): AnyShouldWrapper/*=>example.Issue2040.AnyShouldWrapper#*/

    i/*=>example.Issue2040.Base#i().*/ shouldBe/*=>example.Issue2040.AnyShouldWrapper#shouldBe().*/ 1
  }
}
