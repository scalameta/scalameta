package scala.meta.tests.parsers.dotty

import scala.meta._

class EndMarkerSuite extends BaseDottySuite {

  test("end-marker") {
    runTestAssert[Stat]("end token")(
      Term.EndMarker(Term.Name("token"))
    )
    runTestAssert[Stat]("end match")(
      Term.EndMarker(Term.Name("match"))
    )
  }

  test("end-marker-keyword") {
    val markers =
      List("if", "while", "for", "match", "try", "new", "this", "given", "extension", "val")
    for (m <- markers) {
      parseStat(s"end $m", dialect)
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
    )
  }

  test("end-marker-extension") {
    val code = """|extension (a: Int)
                  |  def b = a + 1
                  |end extension
                  |
                  |type K = Map
                  |""".stripMargin
    runTestAssert[Source](code, assertLayout = None)(
      Source(
        List(
          Defn.ExtensionGroup(
            Nil,
            List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))),
            Defn.Def(
              Nil,
              Term.Name("b"),
              Nil,
              Nil,
              None,
              Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Lit.Int(1)))
            )
          ),
          Term.EndMarker(Term.Name("extension")),
          Defn.Type(Nil, Type.Name("K"), Nil, Type.Name("Map"))
        )
      )
    )
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

  test("if-then-end-ident") {
    val code = """|if limit < end then
                  |   val aa = 1
                  |""".stripMargin
    val output = """|if (limit < end) {
                    |  val aa = 1
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.ApplyInfix(Term.Name("limit"), Term.Name("<"), Nil, List(Term.Name("end"))),
        Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("aa"))), None, Lit.Int(1)))),
        Lit.Unit()
      )
    )
  }

  test("match-with-end") {
    val code = """|val a = this match
                  |      case a =>
                  |         that match
                  |            case b => bb
                  |            case c => cc
                  |         end match
                  |      case b =>
                  |         that match
                  |            case c => cc
                  |            case _ => dd
                  |""".stripMargin
    val output = """|val a = this match {
                    |  case a =>
                    |    that match {
                    |      case b => bb
                    |      case c => cc
                    |    }
                    |    end match
                    |  case b =>
                    |    that match {
                    |      case c => cc
                    |      case _ => dd
                    |    }
                    |}
                    |""".stripMargin
    runTestAssert[Source](code, assertLayout = Some(output))(
      Source(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(Term.Name("a"))),
            None,
            Term.Match(
              Term.This(Name("")),
              List(
                Case(
                  Pat.Var(Term.Name("a")),
                  None,
                  Term.Block(
                    List(
                      Term.Match(
                        Term.Name("that"),
                        List(
                          Case(Pat.Var(Term.Name("b")), None, Term.Name("bb")),
                          Case(Pat.Var(Term.Name("c")), None, Term.Name("cc"))
                        ),
                        Nil
                      ),
                      Term.EndMarker(Term.Name("match"))
                    )
                  )
                ),
                Case(
                  Pat.Var(Term.Name("b")),
                  None,
                  Term.Match(
                    Term.Name("that"),
                    List(
                      Case(Pat.Var(Term.Name("c")), None, Term.Name("cc")),
                      Case(Pat.Wildcard(), None, Term.Name("dd"))
                    ),
                    Nil
                  )
                )
              ),
              Nil
            )
          )
        )
      )
    )
  }

  test("val-with-end") {
    val code = """|object a:
                  |  val (foo, boo) =
                  |    bar
                  |    baz
                  |  end val
                  |  val (foo2, boo3) =
                  |    bar
                  |    baz
                  |  end val
                  |""".stripMargin
    val output = """|object a {
                    |  val (foo, boo) = {
                    |    bar
                    |    baz
                    |  }
                    |  end val
                    |  val (foo2, boo3) = {
                    |    bar
                    |    baz
                    |  }
                    |  end val
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Object(
        Nil,
        Term.Name("a"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Val(
              Nil,
              List(Pat.Tuple(List(Pat.Var(Term.Name("foo")), Pat.Var(Term.Name("boo"))))),
              None,
              Term.Block(List(Term.Name("bar"), Term.Name("baz")))
            ),
            Term.EndMarker(Term.Name("val")),
            Defn.Val(
              Nil,
              List(Pat.Tuple(List(Pat.Var(Term.Name("foo2")), Pat.Var(Term.Name("boo3"))))),
              None,
              Term.Block(List(Term.Name("bar"), Term.Name("baz")))
            ),
            Term.EndMarker(Term.Name("val"))
          ),
          Nil
        )
      )
    )
  }

  test("object-empty-body-end-marker") {
    runTestAssert[Source](
      """|object Foo:
         |end Foo
         |""".stripMargin,
      assertLayout = None
    )(
      Source(
        List(
          Defn.Object(Nil, Term.Name("Foo"), Template(Nil, Nil, Self(Name(""), None), Nil, Nil)),
          Term.EndMarker(Term.Name("Foo"))
        )
      )
    )
  }

  test("class-empty-body-end-marker") {
    runTestAssert[Source](
      """|class Foo:
         |end Foo
         |""".stripMargin,
      assertLayout = None
    )(
      Source(
        List(
          Defn.Class(
            Nil,
            Type.Name("Foo"),
            Nil,
            Ctor.Primary(Nil, Name(""), Nil),
            Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
          ),
          Term.EndMarker(Term.Name("Foo"))
        )
      )
    )
  }

  test("trait-empty-body-end-marker") {
    runTestAssert[Source](
      """|trait Foo:
         |end Foo
         |""".stripMargin,
      assertLayout = None
    )(
      Source(
        List(
          Defn.Trait(
            Nil,
            Type.Name("Foo"),
            Nil,
            Ctor.Primary(Nil, Name(""), Nil),
            Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
          ),
          Term.EndMarker(Term.Name("Foo"))
        )
      )
    )
  }

  test("trait-empty-body-no-end-marker") {
    runTestError[Source](
      """|trait Foo:
         |  trait Baz:
         |  trait Bar:
         |  end Bar
         |end Foo
         |""".stripMargin,
      """|<input>:2: error: expected template body
         |  trait Baz:
         |            ^""".stripMargin
    )
  }

  test("trait-empty-body-outer-end-marker") {
    runTestError[Source](
      """|trait Foo:
         |  trait Bar:
         |end Foo
         |""".stripMargin,
      """|<input>:2: error: expected template body
         |  trait Bar:
         |            ^""".stripMargin
    )
  }

  test("trait-empty-body-no-end-marker") {
    runTestError[Source](
      """|trait Foo:
         |""".stripMargin,
      """|<input>:1: error: expected template body
         |trait Foo:
         |          ^""".stripMargin
    )
  }

}
