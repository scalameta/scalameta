package scala.meta.tests.parsers.dotty

import scala.meta._

class GivenSyntax36Suite extends BaseDottySuite {

  test("named-bounds") {
    val code = """|def reduce[A: Monoid as mm](xs: A): A = ???
                  |""".stripMargin
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      Term.Name("reduce"),
      List(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.BoundsAlias(Type.Name("mm"), Type.Name("Monoid")))
        ))),
        List(Term.ParamClause(List(Term.Param(Nil, Term.Name("xs"), Some(Type.Name("A")), None))))
      )),
      Some(Type.Name("A")),
      Term.Name("???")
    ))
  }

  //
  // class B extends A:
  //   def showMax[X : {Ordering as ordering, Show as show}](x: X, y: X): String =
  test("agregate-bounds") {
    runTestAssert[Stat](
      "trait A { def showMax[X: {Ordering, Show}](x: X, y: X): String}",
      assertLayout = Some("trait A { def showMax[X: Ordering: Show](x: X, y: X): String }")
    )(Defn.Trait(
      Nil,
      Type.Name("A"),
      Type.ParamClause(Nil),
      Ctor.Primary(Nil, Name.Anonymous(), Nil),
      Template(
        None,
        Nil,
        Template.Body(
          None,
          List(Decl.Def(
            Nil,
            Term.Name("showMax"),
            List(Member.ParamClauseGroup(
              Type.ParamClause(List(Type.Param(
                Nil,
                Type.Name("X"),
                Type.ParamClause(Nil),
                Type.Bounds(None, None),
                Nil,
                List(Type.Name("Ordering"), Type.Name("Show"))
              ))),
              List(Term.ParamClause(List(
                Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
                Term.Param(Nil, Term.Name("y"), Some(Type.Name("X")), None)
              )))
            )),
            Type.Name("String")
          ))
        ),
        Nil
      )
    ))
  }
  test("agregate-bounds-named") {
    runTestAssert[Stat](
      "trait A{ def showMax[X : {Ordering as ordering, Show as show}](x: X, y: X): String }",
      assertLayout =
        Some("trait A { def showMax[X: Ordering as ordering: Show as show](x: X, y: X): String }")
    )(Defn.Trait(
      Nil,
      Type.Name("A"),
      Type.ParamClause(Nil),
      Ctor.Primary(Nil, Name.Anonymous(), Nil),
      Template(
        None,
        Nil,
        Template.Body(
          None,
          List(Decl.Def(
            Nil,
            Term.Name("showMax"),
            List(Member.ParamClauseGroup(
              Type.ParamClause(List(Type.Param(
                Nil,
                Type.Name("X"),
                Type.ParamClause(Nil),
                Type.Bounds(None, None),
                Nil,
                List(
                  Type.BoundsAlias(Type.Name("ordering"), Type.Name("Ordering")),
                  Type.BoundsAlias(Type.Name("show"), Type.Name("Show"))
                )
              ))),
              List(Term.ParamClause(List(
                Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
                Term.Param(Nil, Term.Name("y"), Some(Type.Name("X")), None)
              )))
            )),
            Type.Name("String")
          ))
        ),
        Nil
      )
    ))
  }
  test("deferred") {
    runTestAssert[Stat](
      """|trait Sorted:
         |  type Element
         |  given Ord[Element] = compiletime.deferred
         |""".stripMargin,
      assertLayout = Some(
        """|trait Sorted {
           |  type Element
           |  given Ord[Element] = compiletime.deferred
           |}
           |""".stripMargin
      )
    )(Defn.Trait(
      Nil,
      Type.Name("Sorted"),
      Type.ParamClause(Nil),
      Ctor.Primary(Nil, Name.Anonymous(), Nil),
      Template(
        None,
        Nil,
        Template.Body(
          None,
          List(
            Decl.Type(Nil, Type.Name("Element"), Type.ParamClause(Nil), Type.Bounds(None, None)),
            Defn.GivenAlias(
              Nil,
              Name.Anonymous(),
              None,
              Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("Element")))),
              Term.Select(Term.Name("compiletime"), Term.Name("deferred"))
            )
          )
        ),
        Nil
      )
    ))
  }

  test("poly-function") {
    runTestAssert[Stat](
      """|type Comparer = [X: Ord] => (x: X, y: X) => Boolean
         |""".stripMargin
    )(Defn.Type(
      Nil,
      Type.Name("Comparer"),
      Type.ParamClause(Nil),
      Type.PolyFunction(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("X"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.Name("Ord"))
        ))),
        Type.Function(
          Type.FuncParamClause(List(
            Type.TypedParam(Type.Name("x"), Type.Name("X"), Nil),
            Type.TypedParam(Type.Name("y"), Type.Name("X"), Nil)
          )),
          Type.Name("Boolean")
        )
      ),
      Type.Bounds(None, None)
    ))
  }
  test("poly-function-val") {
    runTestAssert[Stat](
      """|val less: Comparer = [X: Ord as ord] => (x: X, y: X) =>
         |  ord.compare(x, y) < 0
         |""".stripMargin,
      assertLayout =
        Some("val less: Comparer = [X: Ord as ord] => (x: X, y: X) => ord.compare(x, y) < 0")
    )(Defn.Val(
      Nil,
      List(Pat.Var(Term.Name("less"))),
      Some(Type.Name("Comparer")),
      Term.PolyFunction(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("X"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.BoundsAlias(Type.Name("ord"), Type.Name("Ord")))
        ))),
        Term.Function(
          Term.ParamClause(List(
            Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None),
            Term.Param(Nil, Term.Name("y"), Some(Type.Name("X")), None)
          )),
          Term.ApplyInfix(
            Term.Apply(
              Term.Select(Term.Name("ord"), Term.Name("compare")),
              Term.ArgClause(List(Term.Name("x"), Term.Name("y")))
            ),
            Term.Name("<"),
            Type.ArgClause(Nil),
            Term.ArgClause(List(Lit.Int(0)))
          )
        )
      )
    ))
  }
}
