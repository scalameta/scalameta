package scala.meta.tests

package quasiquotes

import scala.meta._
import scala.meta.dialects.Scala3

class Scala3SpecificSuccessSuite extends TreeSuiteBase {

  test("multiline - parse when not inlined") {
    def foo(name: Type.Name) =
      q"""
        class $name:
          val a: Int = 1
      """
    assertPositions(
      foo(Type.Name("AAA")),
      """|<?>Defn.Class class AAA { val a: Int = 1 }</?> [0:...class `$name`:...:55)
         |<tparamClause>Type.ParamClause         class `$name`@@:</tparamClause> [22::22)
         |<ctor>Ctor.Primary         class `$name`@@:</ctor> [22::22)
         |<templ>Template { val a: Int = 1 }</templ> [22<:...>48)
         |<body>Template.Body { val a: Int = 1 }</body> [22<:...>48)
         |<stats0>Defn.Val val a: Int = 1</stats0> [34:val a: Int = 1:48)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
      skipFullTree = false
    )
  }

  test("multiline - parse when inlined") {
    inline def foo(name: Type.Name) =
      q"""
        class $name:
          val a: Int = 1
      """
    assertPositions(
      foo(Type.Name("AAA")),
      """|<?>Defn.Class class AAA { val a: Int = 1 }</?> [0:...class `$name`:...:55)
         |<tparamClause>Type.ParamClause         class `$name`@@:</tparamClause> [22::22)
         |<ctor>Ctor.Primary         class `$name`@@:</ctor> [22::22)
         |<templ>Template { val a: Int = 1 }</templ> [22<:...>48)
         |<body>Template.Body { val a: Int = 1 }</body> [22<:...>48)
         |<stats0>Defn.Val val a: Int = 1</stats0> [34:val a: Int = 1:48)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
      skipFullTree = false
    )
  }

  test("single-line - parse when not inlined") {
    def foo() = q"val aaa: Int = 1"
    assertPositions(
      foo(),
      """|<?>Defn.Val val aaa: Int = 1</?> [0:val aaa: Int = 1:16)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
      skipFullTree = false
    )
  }

  test("single-line - parse when inlined") {
    inline def foo() = q"val aaa: Int = 1"
    assertPositions(
      foo(),
      """|<?>Defn.Val val aaa: Int = 1</?> [0:val aaa: Int = 1:16)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
      skipFullTree = false
    )
  }

  test("#4434 quasiquote in braces") {
    val fooTypes = Seq(q"Foo", q"Bar")
    val quoted: Tree = q"""${fooTypes(0)}; "any message""""

    assertTokensAsStructureLines(
      quoted.tokens,
      """|BOF [0..0)
         |Ident(${fooTypes(0)}) [0..16)
         |Semicolon [16..17)
         |Space [17..18)
         |Constant.String(any message) [18..31)
         |EOF [31..31)
         |""".stripMargin
    )
    val pos = quoted.pos
    assertNoDiff(pos.toString, """[0,31) in str(`${fooTypes(0)}`; "any message")""")
    assertNoDiff(pos.text, """`${fooTypes(0)}`; "any message"""")
    assertPositions(
      quoted,
      """|<stats1>Lit.String "any message"</stats1> [18:"any message":31)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true
    )

    val syntax =
      """|{
         |  Foo
         |  "any message"
         |}
         |""".stripMargin
    assertNoDiff(quoted.text, syntax)
    assertNoDiff(quoted.syntax, syntax)
    assertNoDiff(quoted.reprint, syntax)
    assertTree(quoted)(blk(tname("Foo"), lit("any message")))
  }

}
