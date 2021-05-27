package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.dialects.Scala3

class Scala3SyntaxSuite extends BaseDottySuite {

  test("given intOrd: Ord[Int] with \n{ def f(): Int = 1 }") {
    assertEquals(
      templStat(
        "given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
      ).syntax,
      "given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
    )
    val Defn.Given(_, _, _, _, template) =
      templStat(
        "given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
      )
    assertEquals(template.syntax, "Ord[Int] with \n{ def f(): Int = 1 }")
  }

  test("enum C extends A with B { case D }") {
    assertEquals(
      templStat("enum C extends A with B { case D }").syntax,
      "enum C extends A with B { case D }"
    )
    val q"enum $name $template" = q"enum C extends A with B { case D }"
    assertEquals(template.syntax, "extends A with B { case D }")
  }
}
