package scala.meta.tests.parsers.dotty

import scala.meta._

class GivenSyntax36Suite extends BaseDottySuite {

  test("named-bounds") {
    val code = """|def reduce[A: Monoid as mm](xs: A): A = ???
                  |""".stripMargin
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      "reduce",
      List(pparam(Nil, "A", cb = List(Type.BoundsAlias("mm", "Monoid")))),
      List(List(tparam("xs", "A"))),
      Some("A"),
      "???"
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
      pname("A"),
      Nil,
      ctor,
      tpl(Decl.Def(
        Nil,
        "showMax",
        List(pparam(Nil, "X", cb = List("Ordering", "Show"))),
        List(List(tparam("x", "X"), tparam("y", "X"))),
        "String"
      ))
    ))
  }
  test("agregate-bounds-named") {
    runTestAssert[Stat](
      "trait A{ def showMax[X : {Ordering as ordering, Show as show}](x: X, y: X): String }",
      assertLayout =
        Some("trait A { def showMax[X: Ordering as ordering: Show as show](x: X, y: X): String }")
    )(Defn.Trait(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(Decl.Def(
        Nil,
        "showMax",
        List(pparam(
          Nil,
          "X",
          cb = List(Type.BoundsAlias("ordering", "Ordering"), Type.BoundsAlias("show", "Show"))
        )),
        List(List(tparam("x", "X"), tparam("y", "X"))),
        "String"
      ))
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
      pname("Sorted"),
      Nil,
      ctor,
      tpl(
        Decl.Type(Nil, pname("Element"), Nil, noBounds),
        Defn
          .GivenAlias(Nil, anon, None, papply("Ord", "Element"), tselect("compiletime", "deferred"))
      )
    ))
  }

  test("poly-function") {
    runTestAssert[Stat](
      """|type Comparer = [X: Ord] => (x: X, y: X) => Boolean
         |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("Comparer"),
      Nil,
      ppolyfunc(
        pfunc(List(Type.TypedParam("x", "X", Nil), Type.TypedParam("y", "X", Nil)), "Boolean"),
        pparam(Nil, "X", cb = List("Ord"))
      ),
      noBounds
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
      List(patvar("less")),
      Some("Comparer"),
      Term.PolyFunction(
        List(pparam(Nil, "X", cb = List(Type.BoundsAlias("ord", "Ord")))),
        tfunc(
          tinfix(tapply(tselect("ord", "compare"), "x", "y"), "<", lit(0)),
          tparam("x", "X"),
          tparam("y", "X")
        )
      )
    ))
  }

  val body = tplBody(
    Defn.Def(Nil, "compare", Nil, List(List(tparam("x", "Int"), tparam("y", "Int"))), None, "???")
  )

  test("given") {
    runTestAssert[Stat](
      """|given Ord[Int] with
         |   def compare(x: Int, y: Int) = ???
         |""".stripMargin,
      assertLayout = Some("given Ord[Int] with { def compare(x: Int, y: Int) = ??? }")
    )(Defn.Given(Nil, anon, None, tpl(List(init(papply("Ord", "Int"))), body)))
  }

  val bodyBounds = tpl(
    List(init(papply("Ord", papply("List", "A")))),
    List(Defn.Def(
      Nil,
      "compare",
      Nil,
      List(List(tparam("x", papply("List", "A")), tparam("y", papply("List", "A")))),
      None,
      "???"
    ))
  )

  test("given-context") {
    runTestAssert[Stat](
      """|given [A: Ord]: Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout =
        Some("given [A: Ord]: Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }")
    )(Defn.Given(Nil, anon, List(pparam(Nil, "A", cb = List("Ord"))), Nil, bodyBounds))
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
      anon,
      List(pparam("A")),
      List(List(tparamUsing("", papply("Ord", "A")))),
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
      anon,
      List(pparam("A")),
      List(List(tparamUsing("ord", papply("Ord", "A")))),
      bodyBounds
    ))
  }
  test("given-simple-alias") {
    runTestAssert[Stat]("given Ord[Int] = IntOrd()")(
      Defn.GivenAlias(Nil, anon, None, papply("Ord", "Int"), tapply("IntOrd"))
    )
  }

  test("given-alias-context-bound") {
    runTestAssert[Stat]("given [A: Ord]: Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      anon,
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      papply("Ord", papply("List", "A")),
      tapplytype("ListOrd", "A")
    ))
  }

  test("given-alias-context-param") {
    runTestAssert[Stat]("given [A](using Ord[A]): Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      anon,
      List(pparam("A")),
      List(List(tparamUsing("", papply("Ord", "A")))),
      papply("Ord", papply("List", "A")),
      tapplytype("ListOrd", "A")
    ))
  }

  test("given-by-name") {
    runTestAssert[Stat]("given [DummySoItsByName]: Context = curCtx")(
      Defn.GivenAlias(Nil, anon, List(pparam("DummySoItsByName")), Nil, "Context", "curCtx")
    )
  }

  test("given-named") {
    runTestAssert[Stat](
      """|given intOrd: Ord[Int] with
         |   def compare(x: Int, y: Int) = ???
         |""".stripMargin,
      assertLayout = Some("given intOrd: Ord[Int] with { def compare(x: Int, y: Int) = ??? }")
    )(Defn.Given(Nil, "intOrd", None, tpl(List(init(papply("Ord", "Int"))), body)))
  }

  test("given-context-named") {
    runTestAssert[Stat](
      """|given listOrd[A: Ord]: Ord[List[A]] with
         |  def compare(x: List[A], y: List[A]) = ???
         |""".stripMargin,
      assertLayout = Some(
        "given listOrd[A: Ord]: Ord[List[A]] with { def compare(x: List[A], y: List[A]) = ??? }"
      )
    )(Defn.Given(Nil, "listOrd", List(pparam(Nil, "A", cb = List("Ord"))), Nil, bodyBounds))
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
      "listOrd",
      List(pparam("A")),
      List(List(tparamUsing("", papply("Ord", "A")))),
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
      "listOrd",
      List(pparam("A")),
      List(List(tparamUsing("ord", papply("Ord", "A")))),
      bodyBounds
    ))
  }

  test("given-simple-alias-named") {
    runTestAssert[Stat]("given intOrd: Ord[Int] = IntOrd()")(
      Defn.GivenAlias(Nil, "intOrd", None, papply("Ord", "Int"), tapply("IntOrd"))
    )
  }

  test("given-alias-context-bound-named") {
    runTestAssert[Stat]("given listOrd[A: Ord]: Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      "listOrd",
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      papply("Ord", papply("List", "A")),
      tapplytype("ListOrd", "A")
    ))
  }

  test("given-alias-context-param-named") {
    runTestAssert[Stat]("given listOrd[A](using Ord[A]): Ord[List[A]] = ListOrd[A]")(Defn.GivenAlias(
      Nil,
      "listOrd",
      List(pparam("A")),
      List(List(tparamUsing("", papply("Ord", "A")))),
      papply("Ord", papply("List", "A")),
      tapplytype("ListOrd", "A")
    ))
  }

  test("given-by-name-named") {
    runTestAssert[Stat]("given context[DummySoItsByName]: Context = curCtx")(
      Defn.GivenAlias(Nil, "context", List(pparam("DummySoItsByName")), Nil, "Context", "curCtx")
    )
  }
  test("given-abstract-named") {
    runTestAssert[Stat]("given context: Context")(Decl.Given(Nil, "context", None, "Context"))
  }

  // https://docs3.scala-lang.org/sips/sips/typeclasses-syntax.html#7-cleanup-of-given-syntax

  // Simple typeclass
  test("Simple anonymous typeclass, coloneol") {
    val code = """|given Ord[Int]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given Ord[Int] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      tpl(List(init(papply("Ord", "Int"))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Simple anonymous typeclass, braces") {
    val code = """|given Ord[Int] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given Ord[Int] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      tpl(List(init(papply("Ord", "Int"))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Simple named typeclass, coloneol") {
    val code = """|given ord: Ord[Int]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: Ord[Int] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      tpl(List(init(papply("Ord", "Int"))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Simple named typeclass, braces") {
    val code = """|given ord: Ord[Int] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: Ord[Int] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      tpl(List(init(papply("Ord", "Int"))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // Parameterized typeclass with context bound
  test("Parameterized anonymous typeclass with context bound, coloneol") {
    val code = """|given [A : Ord] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given [A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized anonymous typeclass with context bound, braces") {
    val code = """|given [A : Ord] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given [A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with context bound, coloneol") {
    val code = """|given ord: [A : Ord] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord[A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with context bound, braces") {
    val code = """|given ord: [A : Ord] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord[A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // Parameterized typeclass with context bound
  test("Parameterized anonymous typeclass with context bound, coloneol") {
    val code = """|given [A : Ord] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given [A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized anonymous typeclass with context bound, braces") {
    val code = """|given [A : Ord] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given [A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with context bound, coloneol") {
    val code = """|given ord: [A : Ord] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord[A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with context bound, braces") {
    val code = """|given ord: [A : Ord] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord[A: Ord]: Ord[List[A]] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam(Nil, "A", cb = List("Ord"))),
      Nil,
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // Parameterized typeclass with named context parameter
  test("Parameterized anonymous typeclass with named context parameter, coloneol") {
    val code = """|given [A] => (ord: Ord[A]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given [A] => (ord: Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam("A")),
      List(List(tparam("ord", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized anonymous typeclass with named context parameter, braces") {
    val code = """|given [A] => (ord: Ord[A]) => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given [A] => (ord: Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam("A")),
      List(List(tparam("ord", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with named context parameter, coloneol") {
    val code = """|given ord: [A] => (ord: Ord[A]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: [A] => (ord: Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam("A")),
      List(List(tparam("ord", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with named context parameter, braces") {
    val code = """|given ord: [A] => (ord: Ord[A]) => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: [A] => (ord: Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam("A")),
      List(List(tparam("ord", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // Parameterized typeclass with anonymous context parameter
  test("Parameterized anonymous typeclass with anonymous context parameter, coloneol") {
    val code = """|given [A] => (Ord[A]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given [A] => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam("A")),
      List(List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized anonymous typeclass with anonymous context parameter, braces") {
    val code = """|given [A] => (Ord[A]) => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given [A] => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      List(pparam("A")),
      List(List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with anonymous context parameter, coloneol") {
    val code = """|given ord: [A] => (Ord[A]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: [A] => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam("A")),
      List(List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("Parameterized named typeclass with anonymous context parameter, braces") {
    val code = """|given ord: [A] => (Ord[A]) => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: [A] => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      tname("ord"),
      List(pparam("A")),
      List(List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // extra new syntax tests: functions

  test("context a lambda function, coloneol") {
    val code = """|given ord: ([A] =>> Ord[A]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: ([A] =>> Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam("", Type.Lambda(List(pparam("A")), papply("Ord", "A"))))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("context a lambda function, braces") {
    val code = """|given ord: ([A] =>> Ord[A]) => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: ([A] =>> Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam("", Type.Lambda(List(pparam(Nil, "A")), papply("Ord", "A"))))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("context a poly function, coloneol") {
    val code = """|given ord: ([A] => (Ord[A]) => Ord[List[A]]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: ([A] => Ord[A] => Ord[List[A]]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam(
        "",
        ppolyfunc(pfunc(papply("Ord", papply("List", "A")), papply("Ord", "A")), pparam(Nil, "A"))
      ))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("context a poly function, braces") {
    val code = """|given ord: ([A] => (Ord[A]) => Ord[List[A]]) => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: ([A] => Ord[A] => Ord[List[A]]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam(
        "",
        ppolyfunc(pfunc(papply("Ord", papply("List", "A")), papply("Ord", "A")), pparam(Nil, "A"))
      ))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("given type a function, coloneol") {
    val code = """|given ord: [A] => (Ord[A] => Ord[List[A]]):
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord[A]: (Ord[A] => Ord[List[A]]) with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      List(pparam(Nil, "A")),
      Nil,
      tpl(
        List(init(pfunc(papply("Ord", papply("List", "A")), papply("Ord", "A")))),
        List(Defn.Def(Nil, "foo", Nil, None, "???"))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("given type a function, braces") {
    val code = """|given ord: [A] => (Ord[A] => Ord[List[A]]) {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord[A]: (Ord[A] => Ord[List[A]]) with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      List(pparam(Nil, "A")),
      Nil,
      tpl(
        List(init(pfunc(papply("Ord", papply("List", "A")), papply("Ord", "A")))),
        List(Defn.Def(Nil, "foo", Nil, None, "???"))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // extra new syntax tests: param clause first

  test("named, param clause first, coloneol") {
    val code = """|given ord: (a: A) => Ord[A] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: (a: A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam("a", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("named, param clause first, braces") {
    val code = """|given ord: (a: A) => Ord[A] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: (a: A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam("a", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("anonymous, param clause first, coloneol") {
    val code = """|given (a: A) => Ord[A] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given (a: A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("a", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("anonymous, param clause first, braces") {
    val code = """|given (a: A) => Ord[A] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given (a: A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("a", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // extra new syntax tests: func arg types

  test("named, func arg types, coloneol") {
    val code = """|given ord: (A) => Ord[A] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given ord: (A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam("", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("named, func arg types, braces") {
    val code = """|given ord: (A) => Ord[A] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given ord: (A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      "ord",
      Nil,
      List(List(tparam("", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("anonymous, func arg types, coloneol") {
    val code = """|given (A) => Ord[A] => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given (A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("anonymous, func arg types, braces") {
    val code = """|given (A) => Ord[A] => Ord[List[A]] {
                  |  def foo = ???
                  |}
                  |""".stripMargin
    val layout = "given (A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("breaks after arrow 1") {
    val code = """|given (A) =>
                  |  Ord[A] =>
                  |  Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given (A) => (Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("", "A")), List(tparam("", papply("Ord", "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("breaks after arrow 2") {
    val code = """|given (a: A =>
                  |  Ord[A]) => Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given (a: A => Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("a", pfunc(papply("Ord", "A"), "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("breaks after arrow 3") {
    val code = """|given (a: A =>
                  |  Ord[A]) =>
                  |  Ord[List[A]]:
                  |  def foo = ???
                  |""".stripMargin
    val layout = "given (a: A => Ord[A]) => Ord[List[A]] { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("a", pfunc(papply("Ord", "A"), "A")))),
      tpl(List(init(papply("Ord", papply("List", "A")))), List(Defn.Def(Nil, "foo", Nil, None, "???")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("import given is not a given") {
    val code = """|object a:
                  |  import scala.util.chaining.given
                  |  import scala.util.control.{ControlThrowable, NonFatal}
                  |""".stripMargin
    val layout = """|object a {
                    |  import scala.util.chaining.given
                    |  import scala.util.control.{ ControlThrowable, NonFatal }
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      "a",
      tpl(
        Import(
          List(Importer(tselect(tselect("scala", "util"), "chaining"), List(Importee.GivenAll())))
        ),
        Import(List(Importer(
          tselect(tselect("scala", "util"), "control"),
          List(Importee.Name(Name("ControlThrowable")), Importee.Name(Name("NonFatal")))
        )))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("given decl") {
    val code = """|object a:
                  |  given TreeMethods: TreeMethods
                  |  trait TreeMethods {}
                  |""".stripMargin
    val layout = """|object a {
                    |  given TreeMethods: TreeMethods
                    |  trait TreeMethods
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      "a",
      tpl(
        Decl.Given(Nil, "TreeMethods", Nil, "TreeMethods"),
        Defn.Trait(Nil, pname("TreeMethods"), Nil, ctor, tplNoBody())
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4091 given") {
    val code = "given List[Int] => Object {}"
    val layout = "given (List[Int]) => Object {}"
    val tree = Defn.Given(
      Nil,
      anon,
      Nil,
      List(List(tparam("", papply("List", "Int")))),
      tpl(List(init("Object")), Nil)
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
