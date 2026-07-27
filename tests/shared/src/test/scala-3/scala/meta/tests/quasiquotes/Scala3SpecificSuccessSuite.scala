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
      """|<?>Defn.Class class AAA { val a: Int = 1 }</?> [0:...class `{$}name`:...:57)
         |<tparamClause>Type.ParamClause         class `{$}name`@@:</tparamClause> [24::24)
         |<ctor>Ctor.Primary         class `{$}name`@@:</ctor> [24::24)
         |<templ>Template { val a: Int = 1 }</templ> [24<:...>50)
         |<body>Template.Body { val a: Int = 1 }</body> [24<:...>50)
         |<stats0>Defn.Val val a: Int = 1</stats0> [36:val a: Int = 1:50)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
      skipFullTree = false,
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
      """|<?>Defn.Class class AAA { val a: Int = 1 }</?> [0:...class `{$}name`:...:57)
         |<tparamClause>Type.ParamClause         class `{$}name`@@:</tparamClause> [24::24)
         |<ctor>Ctor.Primary         class `{$}name`@@:</ctor> [24::24)
         |<templ>Template { val a: Int = 1 }</templ> [24<:...>50)
         |<body>Template.Body { val a: Int = 1 }</body> [24<:...>50)
         |<stats0>Defn.Val val a: Int = 1</stats0> [36:val a: Int = 1:50)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
      skipFullTree = false,
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
      skipFullTree = false,
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
      skipFullTree = false,
    )
  }

  test("#4434 quasiquote in braces") {
    val fooTypes = Seq(q"Foo", q"Bar")
    val quoted: Tree = q"""${fooTypes(0)}; "any message""""

    assertTokensAsStructureLines(
      quoted.tokens,
      """|BOF [0..0)
         |Ident({$}{fooTypes(0)}) [0..18)
         |Semicolon [18..19)
         |Space [19..20)
         |Constant.String(any message) [20..33)
         |EOF [33..33)
         |""".stripMargin,
    )
    val pos = quoted.pos
    assertNoDiff(pos.toString, """[0,33) in str(`{$}{fooTypes(0)}`; "any message")""")
    assertNoDiff(pos.text, """`{$}{fooTypes(0)}`; "any message"""")
    assertPositions(
      quoted,
      """|<stats1>Lit.String "any message"</stats1> [20:"any message":33)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
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
