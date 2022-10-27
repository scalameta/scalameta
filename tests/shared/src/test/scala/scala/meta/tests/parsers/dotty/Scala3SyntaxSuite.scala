package scala.meta.tests.parsers.dotty

import scala.meta._

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

  test("private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }") {
    assertEquals(
      templStat(
        "private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
      ).syntax,
      "private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
    )
    val Defn.Given(_, _, _, _, template) =
      templStat(
        "private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
      )
    assertEquals(template.syntax, "Ord[Int] with \n{ def f(): Int = 1 }")
  }

  test("enum C extends A with B { case D }") {
    val dialect: Dialect = null
    import dialects.Scala3
    assertEquals(
      templStat("enum C extends A with B { case D }").syntax,
      "enum C extends A with B { case D }"
    )
    val q"enum $name $template" = q"enum C extends A with B { case D }"
    assertEquals(template.syntax, "extends A with B { case D }")
  }

  test("protected enum C extends A with B { case D }") {
    val dialect: Dialect = null
    import dialects.Scala3
    assertEquals(
      templStat("protected enum C extends A with B { case D }").syntax,
      "protected enum C extends A with B { case D }"
    )
    val q"protected enum $name $template" =
      q"protected enum C extends A with B { case D }"
    assertEquals(template.syntax, "extends A with B { case D }")
  }

  test("given intOrd: Ord[Int]") {
    assertEquals(
      templStat("given intOrd: Ord[Int]").syntax,
      "given intOrd: Ord[Int]"
    )
  }

  test("backticked-keywords") {
    assertEquals(
      Term.Name("enum").syntax,
      "`enum`"
    )
    assertEquals(
      Term.Name("given").syntax,
      "`given`"
    )
    assertEquals(
      Term.Name("export").syntax,
      "`export`"
    )
    assertEquals(
      Term.Name("then").syntax,
      "`then`"
    )
    assertEquals(
      Term.Name("?=>").syntax,
      "`?=>`"
    )
  }

}
