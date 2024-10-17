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

  val body = Template.Body(
    None,
    List(Defn.Def(
      Nil,
      Term.Name("compare"),
      List(Member.ParamClauseGroup(
        Type.ParamClause(Nil),
        List(Term.ParamClause(List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
          Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
        )))
      )),
      None,
      Term.Name("???")
    ))
  )

  test("given") {
    runTestAssert[Stat](
      """|given Ord[Int] with
         |   def compare(x: Int, y: Int) = ???
         |""".stripMargin,
      assertLayout = Some("given Ord[Int] with { def compare(x: Int, y: Int) = ??? }")
    )(Defn.Given(
      Nil,
      Name.Anonymous(),
      None,
      Template(
        None,
        List(Init(
          Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("Int")))),
          Name.Anonymous(),
          Nil
        )),
        body,
        Nil
      )
    ))
  }

  val bodyBounds = Template(
    None,
    List(Init(
      Type.Apply(
        Type.Name("Ord"),
        Type.ArgClause(List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))))
      ),
      Name.Anonymous(),
      Nil
    )),
    Template.Body(
      None,
      List(Defn.Def(
        Nil,
        Term.Name("compare"),
        List(Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(Term.ParamClause(List(
            Term.Param(
              Nil,
              Term.Name("x"),
              Some(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))),
              None
            ),
            Term.Param(
              Nil,
              Term.Name("y"),
              Some(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))),
              None
            )
          )))
        )),
        None,
        Term.Name("???")
      ))
    ),
    Nil
  )

  test("given-context") {
    runTestAssert[Stat](
      """|given [A: Ord]: Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout =
        Some("given [A: Ord]: Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }")
    )(Defn.Given(
      Nil,
      Name.Anonymous(),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.Name("Ord"))
        ))),
        Nil
      )),
      bodyBounds
    ))
  }

  test("given-context-using") {
    runTestAssert[Stat](
      """|given [A](using Ord[A]): Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout = Some(
        "given [A](using Ord[A]): Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }"
      )
    )(Defn.Given(
      Nil,
      Name.Anonymous(),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(
          Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
        )),
        List(Term.ParamClause(
          List(Term.Param(
            List(Mod.Using()),
            Name.Anonymous(),
            Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("A"))))),
            None
          )),
          Some(Mod.Using())
        ))
      )),
      bodyBounds
    ))
  }

  test("given-context-using-named") {
    runTestAssert[Stat](
      """|given [A](using ord: Ord[A]): Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout =
        Some("given [A](using ord: Ord[A]): Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }")
    )(Defn.Given(
      Nil,
      Name.Anonymous(),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(
          Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
        )),
        List(Term.ParamClause(
          List(Term.Param(
            List(Mod.Using()),
            Term.Name("ord"),
            Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("A"))))),
            None
          )),
          Some(Mod.Using())
        ))
      )),
      bodyBounds
    ))
  }
  test("given-simple-alias") {
    runTestAssert[Stat]("given Ord[Int] = IntOrd()")(Defn.GivenAlias(
      Nil,
      Name.Anonymous(),
      None,
      Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("Int")))),
      Term.Apply(Term.Name("IntOrd"), Term.ArgClause(Nil))
    ))
  }

  test("given-alias-context-bound") {
    runTestAssert[Stat]("given [A: Ord]: Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      Name.Anonymous(),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.Name("Ord"))
        ))),
        Nil
      )),
      Type.Apply(
        Type.Name("Ord"),
        Type.ArgClause(List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))))
      ),
      Term.ApplyType(Term.Name("ListOrd"), Type.ArgClause(List(Type.Name("A"))))
    ))
  }

  test("given-alias-context-param") {
    runTestAssert[Stat]("given [A](using Ord[A]): Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      Name.Anonymous(),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(
          Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
        )),
        List(Term.ParamClause(
          List(Term.Param(
            List(Mod.Using()),
            Name.Anonymous(),
            Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("A"))))),
            None
          )),
          Some(Mod.Using())
        ))
      )),
      Type.Apply(
        Type.Name("Ord"),
        Type.ArgClause(List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))))
      ),
      Term.ApplyType(Term.Name("ListOrd"), Type.ArgClause(List(Type.Name("A"))))
    ))
  }

  test("given-by-name") {
    runTestAssert[Stat]("given [DummySoItsByName]: Context = curCtx")(Defn.GivenAlias(
      Nil,
      Name.Anonymous(),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("DummySoItsByName"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          Nil
        ))),
        Nil
      )),
      Type.Name("Context"),
      Term.Name("curCtx")
    ))
  }

  test("given-named") {
    runTestAssert[Stat](
      """|given intOrd: Ord[Int] with
         |   def compare(x: Int, y: Int) = ???
         |""".stripMargin,
      assertLayout = Some("given intOrd: Ord[Int] with { def compare(x: Int, y: Int) = ??? }")
    )(Defn.Given(
      Nil,
      Term.Name("intOrd"),
      None,
      Template(
        None,
        List(Init(
          Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("Int")))),
          Name.Anonymous(),
          Nil
        )),
        body,
        Nil
      )
    ))
  }

  test("given-context-named") {
    runTestAssert[Stat](
      """|given listOrd[A: Ord]: Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout = Some(
        "given listOrd[A: Ord]: Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }"
      )
    )(Defn.Given(
      Nil,
      Term.Name("listOrd"),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.Name("Ord"))
        ))),
        Nil
      )),
      bodyBounds
    ))
  }

  test("given-context-using") {
    runTestAssert[Stat](
      """|given listOrd[A](using Ord[A]): Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout =
        Some("given listOrd[A](using Ord[A]): Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }")
    )(Defn.Given(
      Nil,
      Term.Name("listOrd"),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(
          Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
        )),
        List(Term.ParamClause(
          List(Term.Param(
            List(Mod.Using()),
            Name.Anonymous(),
            Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("A"))))),
            None
          )),
          Some(Mod.Using())
        ))
      )),
      bodyBounds
    ))
  }

  test("given-context-using-named") {
    runTestAssert[Stat](
      """|given listOrd[A](using ord: Ord[A]): Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout =
        Some("given listOrd[A](using ord: Ord[A]): Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }")
    )(Defn.Given(
      Nil,
      Term.Name("listOrd"),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(
          Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
        )),
        List(Term.ParamClause(
          List(Term.Param(
            List(Mod.Using()),
            Term.Name("ord"),
            Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("A"))))),
            None
          )),
          Some(Mod.Using())
        ))
      )),
      bodyBounds
    ))
  }

  test("given-simple-alias-named") {
    runTestAssert[Stat]("given intOrd: Ord[Int] = IntOrd()")(Defn.GivenAlias(
      Nil,
      Term.Name("intOrd"),
      None,
      Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("Int")))),
      Term.Apply(Term.Name("IntOrd"), Term.ArgClause(Nil))
    ))
  }

  test("given-alias-context-bound-named") {
    runTestAssert[Stat]("given listOrd[A: Ord]: Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      Term.Name("listOrd"),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          List(Type.Name("Ord"))
        ))),
        Nil
      )),
      Type.Apply(
        Type.Name("Ord"),
        Type.ArgClause(List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))))
      ),
      Term.ApplyType(Term.Name("ListOrd"), Type.ArgClause(List(Type.Name("A"))))
    ))
  }

  test("given-alias-context-param-named") {
    runTestAssert[Stat]("given listOrd[A](using Ord[A]): Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      Term.Name("listOrd"),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(
          Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
        )),
        List(Term.ParamClause(
          List(Term.Param(
            List(Mod.Using()),
            Name.Anonymous(),
            Some(Type.Apply(Type.Name("Ord"), Type.ArgClause(List(Type.Name("A"))))),
            None
          )),
          Some(Mod.Using())
        ))
      )),
      Type.Apply(
        Type.Name("Ord"),
        Type.ArgClause(List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A"))))))
      ),
      Term.ApplyType(Term.Name("ListOrd"), Type.ArgClause(List(Type.Name("A"))))
    ))
  }

  test("given-by-name-named") {
    runTestAssert[Stat]("given context[DummySoItsByName]: Context = curCtx")(Defn.GivenAlias(
      Nil,
      Term.Name("context"),
      Some(Member.ParamClauseGroup(
        Type.ParamClause(List(Type.Param(
          Nil,
          Type.Name("DummySoItsByName"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          Nil
        ))),
        Nil
      )),
      Type.Name("Context"),
      Term.Name("curCtx")
    ))
  }
  test("given-abstract-named") {
    runTestAssert[Stat]("given context: Context")(
      Decl.Given(Nil, Term.Name("context"), None, Type.Name("Context"))
    )
  }
}
