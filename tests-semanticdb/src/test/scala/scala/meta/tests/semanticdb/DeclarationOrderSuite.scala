package scala.meta.tests
package semanticdb

import scala.meta.internal.{semanticdb => s}

// Tests for https://github.com/scalameta/scalameta/issues/1544:
// ClassSignature.declarations must list declared members in source order, even though
// scalac's namer enters a case class's eagerly-created companion module into the owner
// scope right after the class rather than at its source position.
class DeclarationOrderSuite extends SemanticdbSuite {

  private def declarations(doc: s.TextDocument, symbol: String): List[String] = {
    val info = doc.symbols.find(_.symbol == symbol)
    assert(info.nonEmpty, s"no symbol information for $symbol")
    info.get.signature match {
      case sig: s.ClassSignature => sig.declarations.toList.flatMap(_.symlinks)
      case sig => fail(s"expected ClassSignature for $symbol, obtained $sig")
    }
  }

  // Positive: an explicit companion of a case class declared later and non-adjacently
  // must appear at its source position, not in the scope slot the namer created for it.
  targeted(
    """|package do1
       |object <<X>> {
       |  case class A()
       |  class B
       |  object A
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do1/X.A#", "do1/X.B#", "do1/X.A.")),
  )

  // Positive: same with a non-class member between the case class and its companion.
  targeted(
    """|package do2
       |object <<X>> {
       |  case class A()
       |  def m: Int = 1
       |  object A
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do2/X.A#", "do2/X.m().", "do2/X.A.")),
  )

  // Negative: an adjacent explicit companion is already in source order and must not move.
  targeted(
    """|package do3
       |object <<X>> {
       |  case class A()
       |  object A
       |  class B
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do3/X.A#", "do3/X.A.", "do3/X.B#")),
  )

  // Negative: a synthetic companion has no explicit source declaration and keeps its slot.
  targeted(
    """|package do4
       |object <<X>> {
       |  case class A()
       |  class B
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do4/X.A#", "do4/X.A.", "do4/X.B#")),
  )

  // Negative: plain (non-case) classes never had the bug; order must stay source order.
  targeted(
    """|package do5
       |object <<X>> {
       |  class A
       |  class B
       |  object A
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do5/X.A#", "do5/X.B#", "do5/X.A.")),
  )

  // Negative: a companion declared before its case class stays first (source order).
  targeted(
    """|package do6
       |object <<X>> {
       |  object A
       |  case class A()
       |  class B
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do6/X.A.", "do6/X.A#", "do6/X.B#")),
  )

  // Positive: eager companion creation is not case-class specific — a class with default
  // constructor parameters also gets an eagerly-entered companion (to hold the default getters),
  // so an explicit companion declared later must float just the same.
  targeted(
    """|package do8
       |object <<X>> {
       |  class A(x: Int = 1)
       |  class B
       |  object A
       |}
       |""".stripMargin,
    (doc, x) => assertEquals(declarations(doc, x), List("do8/X.A#", "do8/X.B#", "do8/X.A.")),
  )

  // Positive: two companions whose objects are declared in the reverse order of their classes
  // must each land at their own source position, not merely follow their class.
  targeted(
    """|package do9
       |object <<X>> {
       |  case class A()
       |  case class B()
       |  object B
       |  object A
       |}
       |""".stripMargin,
    (doc, x) =>
      assertEquals(declarations(doc, x), List("do9/X.A#", "do9/X.B#", "do9/X.B.", "do9/X.A.")),
  )

  // Negative: the spec layout for a template with ctor params — param fields/getters/setters
  // first, then the primary ctor, then body members in source order — is untouched.
  targeted(
    """|package do7
       |class <<K>>(val p1: Int, var p2: String) {
       |  def m1: Int = 1
       |  val v1: Int = 2
       |  var v2: Int = 3
       |  def m2: Int = 4
       |}
       |""".stripMargin,
    (doc, k) =>
      assertEquals(
        declarations(doc, k),
        List(
          "do7/K#p1.",
          "do7/K#p2().",
          "do7/K#`p2_=`().",
          "do7/K#`<init>`().",
          "do7/K#m1().",
          "do7/K#v1.",
          "do7/K#v2().",
          "do7/K#`v2_=`().",
          "do7/K#m2().",
        ),
      ),
  )
}
