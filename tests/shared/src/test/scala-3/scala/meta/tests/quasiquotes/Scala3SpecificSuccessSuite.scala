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
      """|<?>Defn.Class class AAA { val a: Int = 1 }</?> [0:...class $name:...:53)
         |<tparamClause>Type.ParamClause         class $name@@:</tparamClause> [20::20)
         |<ctor>Ctor.Primary         class $name@@:</ctor> [20::20)
         |<templ>Template { val a: Int = 1 }</templ> [20<:...>46)
         |<body>Template.Body { val a: Int = 1 }</body> [20<:...>46)
         |<stats0>Defn.Val val a: Int = 1</stats0> [32:val a: Int = 1:46)
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
      """|<?>Defn.Class class AAA { val a: Int = 1 }</?> [0:...class $name:...:53)
         |<tparamClause>Type.ParamClause         class $name@@:</tparamClause> [20::20)
         |<ctor>Ctor.Primary         class $name@@:</ctor> [20::20)
         |<templ>Template { val a: Int = 1 }</templ> [20<:...>46)
         |<body>Template.Body { val a: Int = 1 }</body> [20<:...>46)
         |<stats0>Defn.Val val a: Int = 1</stats0> [32:val a: Int = 1:46)
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
         |Ident($) [0..1)
         |LeftBrace [1..2)
         |Ident(fooTypes) [2..10)
         |LeftParen [10..11)
         |Constant.Int(0) [11..12)
         |""".stripMargin
    )
    val pos = quoted.pos
    assertNoDiff(pos.toString, """[0..12) in Input.String("${fooTypes(0)}; "any message"")""")
    assertNoDiff(pos.text, """${fooTypes(0""")
    assertPositions(
      quoted,
      """|<stats1>Lit.String (</stats1> [10:(:11)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true
    )

    val syntax =
      """|{
         |  Foo
         |  (
         |}
         |""".stripMargin
    assertNoDiff(quoted.text, syntax)
    assertNoDiff(quoted.syntax, syntax)
    assertNoDiff(quoted.reprint, syntax)
    assertTree(quoted)(blk(tname("Foo"), lit("any message")))
  }

}
