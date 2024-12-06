package scala.meta.tests.prettyprinters.dotty

import scala.meta._

class SyntacticSuite extends scala.meta.tests.parsers.ParseSuite {

  private implicit val dialect: Dialect = dialects.Scala3

  test("literalTypes") {
    assertEquals(
      dialects.Scala3("val a: \"42\" = \"42\"").parse[Stat].get.syntax,
      "val a: \"42\" = \"42\""
    )
    assertEquals(pat("_: 42").reprint, "_: 42")
    assertEquals(pat("_: 42f").reprint, "_: 42f")
    assertEquals(pat("_: 42d").reprint, "_: 42d")
    assertEquals(pat("_: 42.0f").reprint, "_: 42.0f")
    assertEquals(pat("_: 42.0d").reprint, "_: 42.0d")
    assertEquals(pat("_: 42L").reprint, "_: 42L")
    assertEquals(pat("_: true").reprint, "_: true")
    assertEquals(pat("_: false").reprint, "_: false")
  }

  test("case List[_](xs @ _*): scala31") {
    implicit val dialect = dialects.Scala31
    checkTree(pat("List[_](xs @ _*)"), "List[_](xs @ _*)")(Pat.Extract(
      Term.ApplyType(tname("List"), List(Type.Wildcard(Type.Bounds(None, None)))),
      List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard()))
    ))
  }

  test("case List[_](xs @ _*): scala3future") {
    implicit val dialect = dialects.Scala3Future
    checkTree(pat("List[_](xs @ _*)"), "List[_](xs @ _*)")(Pat.Extract(
      Term.ApplyType(tname("List"), List(Type.AnonymousParam(None))),
      List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard()))
    ))
  }

  test("#2699 method declaration with multiple named 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using y: String, z: Boolean): String").reprint,
      "def foo(x: Int)(using y: String, z: Boolean): String"
    )
  }

  test("#2699 method definition with multiple named 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using y: String, z: Boolean) = x").reprint,
      "def foo(x: Int)(using y: String, z: Boolean) = x"
    )
  }

  test("#2699 primary constructor with multiple named 'using' params") {
    assertEquals(
      templStat("class C(x: Int)(using y: String, z: Boolean)").reprint,
      "class C(x: Int)(using y: String, z: Boolean)"
    )
  }

  test("#2699 secondary constructor with multiple named 'using' params") {
    assertEquals(
      templStat(
        "class C(x: Int) { def this(x: String)(using y: String, z: Boolean) = this(x.toInt) }"
      ).reprint,
      "class C(x: Int) { def this(x: String)(using y: String, z: Boolean) = this(x.toInt) }"
    )
  }

  test("#2699 method declaration with multiple anonymous 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using String, Boolean): String").reprint,
      "def foo(x: Int)(using String, Boolean): String"
    )
  }

  test("#2699 method definition with multiple anonymous 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using String, Boolean) = x").reprint,
      "def foo(x: Int)(using String, Boolean) = x"
    )
  }

  test("#2699 primary constructor with multiple anonymous 'using' params") {
    assertEquals(
      templStat("class C(x: Int)(using String, Boolean)").reprint,
      "class C(x: Int)(using String, Boolean)"
    )
  }

  test("#2699 secondary constructor with multiple anonymous 'using' params") {
    assertEquals(
      templStat("class C(x: Int) { def this(x: String)(using String, Boolean) = this(x.toInt) }")
        .reprint,
      "class C(x: Int) { def this(x: String)(using String, Boolean) = this(x.toInt) }"
    )
  }

}
