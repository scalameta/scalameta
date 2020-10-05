package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class EndMarkerSuite extends BaseDottySuite {

  implicit val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)

  val parseSource: String => Source = code => source(code)(dialects.Dotty)

  test("end-marker") {
    runTestAssert[Stat]("end token")(
      Term.EndMarker(Term.Name("token"))
    )
  }

  test("end-marker-keyword") {
    val markers =
      List("if", "while", "for", "match", "try", "new", "this", "given", "extension", "val")
    for (m <- markers) {
      parseTempl(s"end ${m}")
    }
  }

  test("end-marker-toplevel") {
    val code = """|object a:
                  |  init()
                  |end a
                  |
                  |type K = Map
                  |""".stripMargin
    runTestAssert[Source](code, assertLayout = None)(
      Source(
        List(
          Defn.Object(
            Nil,
            Term.Name("a"),
            Template(Nil, Nil, Self(Name(""), None), List(Term.Apply(Term.Name("init"), Nil)))
          ),
          Term.EndMarker(Term.Name("a")),
          Defn.Type(Nil, Type.Name("K"), Nil, Type.Name("Map"))
        )
      )
    )(parseSource)
  }

  test("end-nomarker") {
    runTestAssert[Stat]("lista append end")(
      Term.ApplyInfix(Term.Name("lista"), Term.Name("append"), Nil, List(Term.Name("end")))
    )

    runTestAssert[Stat]("lista end 3")(
      Term.ApplyInfix(Term.Name("lista"), Term.Name("end"), Nil, List(Lit.Int(3)))
    )

    runTestAssert[Stat]("end + 3")(
      Term.ApplyInfix(Term.Name("end"), Term.Name("+"), Nil, List(Lit.Int(3)))
    )

    val code = """|def a: B = {
                  |  b append end
                  |  b
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(
      Defn.Def(
        Nil,
        Term.Name("a"),
        Nil,
        Nil,
        Some(Type.Name("B")),
        Term.Block(
          List(
            Term.ApplyInfix(Term.Name("b"), Term.Name("append"), Nil, List(Term.Name("end"))),
            Term.Name("b")
          )
        )
      )
    )
  }

  test("end-for-no-indent") {
    // to make parser more permissive 'end' is treated as independent statement
    // that doesn't need to be bound to any indentation
    val code = """|
                  |def a(): Unit = {
                  |  end for
                  |  val x = 3
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Def(
        Nil,
        Term.Name("a"),
        Nil,
        List(List()),
        Some(Type.Name("Unit")),
        Term.Block(
          List(
            Term.EndMarker(Term.Name("for")),
            Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(3))
          )
        )
      )
    )
  }

}
