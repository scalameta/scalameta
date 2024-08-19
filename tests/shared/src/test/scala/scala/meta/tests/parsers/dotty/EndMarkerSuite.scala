package scala.meta.tests.parsers.dotty

import scala.meta._

class EndMarkerSuite extends BaseDottySuite {

  test("end-marker") {
    runTestAssert[Stat]("end token")(Term.EndMarker(tname("token")))
    runTestAssert[Stat]("end match")(Term.EndMarker(tname("match")))
  }

  test("end-marker-keyword") {
    val markers =
      List("if", "while", "for", "match", "try", "new", "this", "given", "extension", "val")
    for (m <- markers) parseStat(s"end $m")
  }

  test("end-marker-toplevel") {
    val code = """|object a:
                  |  init()
                  |end a
                  |
                  |type K = Map
                  |""".stripMargin
    runTestAssert[Source](code, assertLayout = None)(Source(List(
      Defn.Object(Nil, tname("a"), tpl(Term.Apply(tname("init"), Nil))),
      Term.EndMarker(tname("a")),
      Defn.Type(Nil, pname("K"), Nil, pname("Map"))
    )))
  }

  test("end-marker-extension") {
    val code = """|extension (a: Int)
                  |  def b = a + 1
                  |end extension
                  |
                  |type K = Map
                  |""".stripMargin
    runTestAssert[Source](code, assertLayout = None)(Source(List(
      Defn.ExtensionGroup(
        Nil,
        List(List(tparam("a", "Int"))),
        Defn.Def(
          Nil,
          tname("b"),
          Nil,
          Nil,
          None,
          Term.ApplyInfix(tname("a"), tname("+"), Nil, List(int(1)))
        )
      ),
      Term.EndMarker(tname("extension")),
      Defn.Type(Nil, pname("K"), Nil, pname("Map"))
    )))
  }

  test("end-nomarker") {
    runTestAssert[Stat]("lista append end")(
      Term.ApplyInfix(tname("lista"), tname("append"), Nil, List(tname("end")))
    )

    runTestAssert[Stat]("lista end 3")(Term.ApplyInfix(tname("lista"), tname("end"), Nil, List(int(3))))

    runTestAssert[Stat]("end + 3")(Term.ApplyInfix(tname("end"), tname("+"), Nil, List(int(3))))

    val code = """|def a: B = {
                  |  b append end
                  |  b
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      tname("a"),
      Nil,
      Nil,
      Some(pname("B")),
      Term
        .Block(List(Term.ApplyInfix(tname("b"), tname("append"), Nil, List(tname("end"))), tname("b")))
    ))
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
    runTestAssert[Stat](code, assertLayout = None)(Defn.Def(
      Nil,
      tname("a"),
      Nil,
      List(List()),
      Some(pname("Unit")),
      Term.Block(
        List(Term.EndMarker(tname("for")), Defn.Val(Nil, List(Pat.Var(tname("x"))), None, int(3)))
      )
    ))
  }

  test("if-then-end-ident") {
    val code = """|if limit < end then
                  |   val aa = 1
                  |""".stripMargin
    val output = """|if (limit < end) {
                    |  val aa = 1
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.If(
      Term.ApplyInfix(tname("limit"), tname("<"), Nil, List(tname("end"))),
      Term.Block(List(Defn.Val(Nil, List(Pat.Var(tname("aa"))), None, int(1)))),
      Lit.Unit()
    ))
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
    runTestAssert[Source](code, assertLayout = Some(output))(Source(List(Defn.Val(
      Nil,
      List(Pat.Var(tname("a"))),
      None,
      Term.Match(
        Term.This(anon),
        List(
          Case(
            Pat.Var(tname("a")),
            None,
            Term.Block(List(
              Term.Match(
                tname("that"),
                List(
                  Case(Pat.Var(tname("b")), None, tname("bb")),
                  Case(Pat.Var(tname("c")), None, tname("cc"))
                ),
                Nil
              ),
              Term.EndMarker(tname("match"))
            ))
          ),
          Case(
            Pat.Var(tname("b")),
            None,
            Term.Match(
              tname("that"),
              List(
                Case(Pat.Var(tname("c")), None, tname("cc")),
                Case(Pat.Wildcard(), None, tname("dd"))
              ),
              Nil
            )
          )
        ),
        Nil
      )
    ))))
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
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Object(
      Nil,
      tname("a"),
      tpl(
        Defn.Val(
          Nil,
          List(Pat.Tuple(List(Pat.Var(tname("foo")), Pat.Var(tname("boo"))))),
          None,
          Term.Block(List(tname("bar"), tname("baz")))
        ),
        Term.EndMarker(tname("val")),
        Defn.Val(
          Nil,
          List(Pat.Tuple(List(Pat.Var(tname("foo2")), Pat.Var(tname("boo3"))))),
          None,
          Term.Block(List(tname("bar"), tname("baz")))
        ),
        Term.EndMarker(tname("val"))
      )
    ))
  }

  test("object-empty-body-end-marker") {
    runTestAssert[Source](
      """|object Foo:
         |end Foo
         |""".stripMargin,
      assertLayout = None
    )(Source(List(Defn.Object(Nil, tname("Foo"), EmptyTemplate()), Term.EndMarker(tname("Foo")))))
  }

  test("class-empty-body-end-marker") {
    runTestAssert[Source](
      """|class Foo:
         |end Foo
         |""".stripMargin,
      assertLayout = None
    )(Source(List(
      Defn.Class(Nil, pname("Foo"), Nil, EmptyCtor(), EmptyTemplate()),
      Term.EndMarker(tname("Foo"))
    )))
  }

  test("trait-empty-body-end-marker") {
    runTestAssert[Source](
      """|trait Foo:
         |end Foo
         |""".stripMargin,
      assertLayout = None
    )(Source(List(
      Defn.Trait(Nil, pname("Foo"), Nil, EmptyCtor(), EmptyTemplate()),
      Term.EndMarker(tname("Foo"))
    )))
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

  test("#3366 not an end marker") {
    val code = """|x.end match
                  |    case _ => ()
                  |""".stripMargin
    val layout = """|x.end match {
                    |  case _ => ()
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Term.Match(
      Term.Select(tname("x"), tname("end")),
      List(Case(Pat.Wildcard(), None, Lit.Unit())),
      Nil
    ))
  }

  test("scalafmt#4098") {
    val code = """|class FmtTest:
                  |
                  |  private val const = 3
                  |
                  |  def testBrokenMatch(s: String) =
                  |    s match
                  |      case "hello" =>
                  |        val a = 1
                  |    end match
                  |  
                  |end FmtTest
                  |""".stripMargin
    val layout = """|class FmtTest {
                    |  private val const = 3
                    |  def testBrokenMatch(s: String) = {
                    |    s match {
                    |      case "hello" =>
                    |        val a = 1
                    |    }
                    |    end match
                    |  }
                    |}
                    |end FmtTest
                    |""".stripMargin
    val tree = Source(List(
      Defn.Class(
        Nil,
        pname("FmtTest"),
        Nil,
        ctor,
        tpl(
          Defn.Val(List(Mod.Private(anon)), List(Pat.Var(tname("const"))), None, lit(3)),
          Defn.Def(
            Nil,
            tname("testBrokenMatch"),
            Nil,
            List(List(tparam("s", "String"))),
            None,
            blk(
              Term.Match(
                tname("s"),
                List(
                  Case(lit("hello"), None, blk(Defn.Val(Nil, List(Pat.Var(tname("a"))), None, lit(1))))
                ),
                Nil
              ),
              Term.EndMarker(tname("match"))
            )
          )
        )
      ),
      Term.EndMarker(tname("FmtTest"))
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("def body contains end, but not end marker") {
    val code = """|object A:
                  |  def foo = bar == end ||
                  |    baz
                  |""".stripMargin
    val layout = "object A { def foo = bar == end || baz }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("foo"),
        Nil,
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(tname("bar"), tname("=="), Nil, List(tname("end"))),
          tname("||"),
          Nil,
          List(tname("baz"))
        )
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
