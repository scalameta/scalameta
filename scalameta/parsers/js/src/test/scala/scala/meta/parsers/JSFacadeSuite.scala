package scala.meta.tests
package parsers

import org.scalatest._
import scala.scalajs.js
import scala.collection.mutable
import scala.meta.parsers._

class JSFacadeSuite extends FunSuite {

  private[this] val d = js.Dictionary
  private[this] val a = js.Array
  private[this] def pos(s: Int, e: Int) = d("start" -> s, "end" -> e)

  private[this] def toJSON(a: js.Dictionary[Any]): String =
    js.JSON.stringify(a.asInstanceOf[js.Any], space = 2)

  private[this] def check(a: js.Dictionary[Any], b: js.Dictionary[Any]) = {
    assert(toJSON(a) == toJSON(b))
  }

  test("parseSource") {
    val code =
      s"""|object Main {
          |  def main(args: Array[String]): Unit =
          |    println("Hello, World")
          |}""".stripMargin
    val parsed = JSFacade.parseSource(code)
    val expected = d(
      "type" -> "Source",
      "pos" -> pos(0, 83),
      "stats" -> a(
        d(
          "type" -> "Defn.Object",
          "pos" -> pos(0, 83),
          "mods" -> a(),
          "name" -> d(
            "type" -> "Term.Name",
            "pos" -> pos(7, 11),
            "value" -> "Main"
          ),
          "templ" -> d(
            "type" -> "Template",
            "pos" -> pos(12, 83),
            "early" -> a(),
            "inits" -> a(),
            "self" -> d(
              "type" -> "Self",
              "pos" -> pos(16, 16),
              "name" -> d(
                "type" -> "Name.Anonymous",
                "pos" -> pos(16, 16),
                "value" -> ""
              )
            ),
            "stats" -> a(
              d(
                "type" -> "Defn.Def",
                "pos" -> pos(16, 81),
                "mods" -> a(),
                "name" -> d(
                  "type" -> "Term.Name",
                  "pos" -> pos(20, 24),
                  "value" -> "main"
                ),
                "tparams" -> a(),
                "paramss" -> a(
                  a(
                    d(
                      "type" -> "Term.Param",
                      "pos" -> pos(25, 44),
                      "mods" -> a(),
                      "name" -> d(
                        "type" -> "Term.Name",
                        "pos" -> pos(25, 29),
                        "value" -> "args"
                      ),
                      "decltpe" -> d(
                        "type" -> "Type.Apply",
                        "pos" -> pos(31, 44),
                        "tpe" -> d(
                          "type" -> "Type.Name",
                          "pos" -> pos(31, 36),
                          "value" -> "Array"
                        ),
                        "args" -> a(
                          d(
                            "type" -> "Type.Name",
                            "pos" -> pos(37, 43),
                            "value" -> "String"
                          )
                        )
                      )
                    )
                  )
                ),
                "decltpe" -> d(
                  "type" -> "Type.Name",
                  "pos" -> pos(47, 51),
                  "value" -> "Unit"
                ),
                "body" -> d(
                  "type" -> "Term.Apply",
                  "pos" -> pos(58, 81),
                  "fun" -> d(
                    "type" -> "Term.Name",
                    "pos" -> pos(58, 65),
                    "value" -> "println"
                  ),
                  "args" -> a(
                    d(
                      "type" -> "Lit.String",
                      "pos" -> pos(66, 80),
                      "value" -> "Hello, World",
                      "syntax" -> """"Hello, World""""
                    )
                  )
                )
              )
            )
          )
        )
      )
    ).asInstanceOf[js.Dictionary[Any]]

    check(parsed, expected)
  }

  private[this] def lit[A](tpe: String, value: A, syntax: String, pos: js.Dictionary[Int]) =
    d(
      "type" -> tpe,
      "pos" -> pos,
      "value" -> value,
      "syntax" -> syntax
    )

  test("parse Lit.Int") {
    val parsed = JSFacade.parseStat("42")
    val expected = lit("Lit.Int", 42, "42", pos(0, 2))
    check(parsed, expected)
  }

  // Ignored because of
  // https://github.com/scalameta/scalameta/issues/961
  ignore("parse Lit.Double") {
    val parsed = JSFacade.parseStat("42.2")
    val expected = lit("Lit.Double", 42.2, "42.2", pos(0, 4))
    check(parsed, expected)
  }

  test("parse Lit.Float") {
    val parsed = JSFacade.parseStat("42.2f")
    val expected = lit("Lit.Float", 42.2f, "42.2f", pos(0, 5))
    check(parsed, expected)
  }

  test("parse Lit.Char") {
    val parsed = JSFacade.parseStat("'a'")
    val expected = lit("Lit.Char", "a", "'a'", pos(0, 3))
    check(parsed, expected)
  }

  test("parse Lit.Long") {
    val parsed = JSFacade.parseStat("42L")
    val expected = lit("Lit.Long", 42, "42L", pos(0, 3))
    check(parsed, expected)
  }

  test("parse Lit.Boolean") {
    val parsed = JSFacade.parseStat("true")
    val expected = lit("Lit.Boolean", true, "true", pos(0, 4))
    check(parsed, expected)
  }

  test("parse Lit.String") {
    val parsed = JSFacade.parseStat(""""42"""")
    val expected = lit("Lit.String", "42", """"42"""", pos(0, 4))
    check(parsed, expected)
  }

  test("parse Lit.Symbol") {
    val parsed = JSFacade.parseStat("'foo")
    val expected = lit("Lit.Symbol", "foo", "'foo", pos(0, 4))
    check(parsed, expected)
  }

  test("parse Name") {
    val parsed = JSFacade.parseStat("foo")
    val expected = d(
      "type" -> "Term.Name",
      "pos" -> pos(0, 3),
      "value" -> "foo"
    ).asInstanceOf[js.Dictionary[Any]]
    check(parsed, expected)
  }

  test("default dialect is Scala 2.11") {
    val code =
      """|List(
         |  1,
         |  2,
         |)""".stripMargin
    val parsedDefaultDialect = JSFacade.parseStat(code)
    val expected = d(
      "error" -> "illegal start of simple expression",
      "pos" -> pos(16, 17),
      "lineNumber" -> 3,
      "columnNumber" -> 0
    ).asInstanceOf[js.Dictionary[Any]]
    check(parsedDefaultDialect, expected)
  }

  test("inexisting dialects report an error") {
    val code =
      """|List(
         |  1,
         |  2,
         |)""".stripMargin
    val parsedDefaultDialect = JSFacade.parseStat(code, js.Dictionary("dialect" -> "wrong"))
    val expected = d(
      "error" -> "'wrong' is not a valid dialect."
    ).asInstanceOf[js.Dictionary[Any]]
    check(parsedDefaultDialect, expected)
  }

  test("can specify dialect") {
    val code =
      """|List(
         |  1,
         |  2,
         |)""".stripMargin
    val parsedDefaultDialect = JSFacade.parseStat(code, js.Dictionary("dialect" -> "Scala212"))
    val expected = d(
      "type" -> "Term.Apply",
      "pos" -> pos(0, 17),
      "fun" -> d(
        "type" -> "Term.Name",
        "pos" -> pos(0, 4),
        "value" -> "List"
      ),
      "args" -> a(
        lit("Lit.Int", 1, "1", pos(8, 9)),
        lit("Lit.Int", 2, "2", pos(13, 14))
      )
    ).asInstanceOf[js.Dictionary[Any]]
    check(parsedDefaultDialect, expected)
  }

}

