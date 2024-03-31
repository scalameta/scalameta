package scala.meta.tests.parsers.dotty

import scala.meta._

class Scala3SyntaxSuite extends BaseDottySuite {

  test("given intOrd: Ord[Int] with \n{ def f(): Int = 1 }") {
    assertEquals(
      templStat("given intOrd: Ord[Int] with \n{ def f(): Int = 1 }").syntax,
      "given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
    )

    matchSubStructure[Stat](
      "given intOrd: Ord[Int] with \n{ def f(): Int = 1 }",
      { case givenDefn: Defn.Given =>
        assertEquals(givenDefn.templ.syntax, "Ord[Int] with \n{ def f(): Int = 1 }")
      }
    )
  }

  test("private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }") {
    assertEquals(
      templStat("private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }").syntax,
      "private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }"
    )

    matchSubStructure[Stat](
      "private final given intOrd: Ord[Int] with \n{ def f(): Int = 1 }",
      { case givenDefn: Defn.Given =>
        assertEquals(givenDefn.templ.syntax, "Ord[Int] with \n{ def f(): Int = 1 }")
      }
    )
  }

  test("enum C extends A with B { case D }") {
    val dialect: Dialect = null
    import dialects.Scala3
    assertEquals(
      templStat("enum C extends A with B { case D }").syntax,
      "enum C extends A with B { case D }"
    )
    stat("enum C extends A with B { case D }") match {
      case Defn.Enum(_, _, _, _, template) =>
        assertEquals(template.syntax, "extends A with B { case D }")
      case _ => fail("Should parse as enumCaseDef")
    }
  }

  test("protected enum C extends A with B { case D }") {
    val dialect: Dialect = null
    import dialects.Scala3
    assertEquals(
      templStat("protected enum C extends A with B { case D }").syntax,
      "protected enum C extends A with B { case D }"
    )
    stat("protected enum C extends A with B { case D }") match {
      case Defn.Enum(_, _, _, _, template) =>
        assertEquals(template.syntax, "extends A with B { case D }")
      case _ => fail("Should parse as enum")
    }
  }

  test("given intOrd: Ord[Int]") {
    assertEquals(templStat("given intOrd: Ord[Int]").syntax, "given intOrd: Ord[Int]")
  }

  test("backticked-keywords") {
    assertEquals(tname("enum").syntax, "`enum`")
    assertEquals(tname("given").syntax, "`given`")
    assertEquals(tname("export").syntax, "`export`")
    assertEquals(tname("then").syntax, "`then`")
    assertEquals(tname("?=>").syntax, "`?=>`")
  }

}
