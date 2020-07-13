package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class GivenUsingSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)

  /**
   * For checking examples in repl declare:
   *  trait Ord[T] { def f(): Int }
   *
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/contextual/givens.html
   *  https://dotty.epfl.ch/docs/reference/contextual/using-clauses.html
   *  https://dotty.epfl.ch/docs/reference/contextual/given-imports.html
   */
  // ---------------------------------
  // GIVEN
  // ---------------------------------

  val defone = Defn.Def(Nil, tname("f"), Nil, List(Nil), Some(pname("Int")), int(1))

  test("given-named") {
    runTestAssert[Stat]("given intOrd as Ord[Int] { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        pname("intOrd"),
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(pname("Int"))),
        tpl(List(defone))
      )
    )
  }

  test("given-named-newline") {
    runTestAssert[Stat]("given intOrd as Ord[Int] \n { def f(): Int = 1 }", assertLayout = None)(
      Defn.Given(
        Nil,
        pname("intOrd"),
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(pname("Int"))),
        tpl(List(defone))
      )
    )
  }

  test("given-anonymous") {
    runTestAssert[Stat]("given Ord[Int] { def f(): Int = 1 }")(
      Defn
        .Given(Nil, anon, Nil, Nil, Type.Apply(pname("Ord"), List(pname("Int"))), tpl(List(defone)))
    )
  }

  test("given-override-def") {
    runTestAssert[Stat]("given intOrd as Ord[Int] { override def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        pname("intOrd"),
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(pname("Int"))),
        tpl(List(defone.copy(mods = List(Mod.Override()))))
      )
    )
  }

  test("given-self") {
    runTestAssert[Stat]("given intOrd as Ord[Int] { current => }")(
      Defn.Given(
        Nil,
        pname("intOrd"),
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(pname("Int"))),
        Template(Nil, Nil, Self(Term.Name("current"), None), Nil)
      )
    )
  }

  test("given-selftype-error".ignore) {
    runTestError(
      "given intOrd as Ord[Int] { current: Ord[Int] => }",
      "objects must not have a self type"
    )
  }

  test("given-no-block") {
    runTestAssert[Stat]("given intOrd as Ord[Int]")(
      Defn.Given(
        Nil,
        pname("intOrd"),
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(pname("Int"))),
        tpl(Nil)
      )
    )
  }

  test("given-anonymous-no-block") {
    runTestAssert[Stat]("given Ord[Int]")(
      Defn.Given(Nil, anon, Nil, Nil, Type.Apply(pname("Ord"), List(pname("Int"))), tpl(Nil))
    )
  }

  test("given-generic-named") {
    runTestAssert[Stat]("given listOrd[T] as Ord[List[T]] { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        pname("listOrd"),
        List(pparam("T")),
        Nil,
        Type.Apply(pname("Ord"), List(Type.Apply(pname("List"), List(pname("T"))))),
        tpl(List(defone))
      )
    )
  }

  test("given-generic-anonymous") {
    runTestAssert[Stat]("given Ord[List[T]] { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        anon,
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(Type.Apply(pname("List"), List(pname("T"))))),
        tpl(List(defone))
      )
    )
  }

  test("given-depend-given-named") {
    runTestAssert[Stat]("given setOrd[T](using ord: Ord[T]) as Ord[Set[T]]")(
      Defn.Given(
        Nil,
        pname("setOrd"),
        List(pparam("T")),
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              tname("ord"),
              Some(Type.Apply(pname("Ord"), List(pname("T")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(Type.Apply(pname("Set"), List(pname("T"))))),
        tpl(Nil)
      )
    )
  }

  test("given-depend-given-anonymous") {
    runTestAssert[Stat]("given [T](using ord: Ord[T]) as Ord[Set[T]]")(
      Defn.Given(
        Nil,
        anon,
        List(pparam("T")),
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              tname("ord"),
              Some(Type.Apply(pname("Ord"), List(pname("T")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(Type.Apply(pname("Set"), List(pname("T"))))),
        tpl(Nil)
      )
    )
  }

  test("given-depend-given-anonymous-using") {
    runTestAssert[Stat]("given (using Ord[String]) as Ord[Int]")(
      Defn.Given(
        Nil,
        anon,
        Nil,
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              anon,
              Some(Type.Apply(pname("Ord"), List(pname("String")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(pname("Int"))),
        tpl(Nil)
      )
    )
  }

  test("given-inline") {
    runTestAssert[Stat]("inline given intOrd as Ord[Int] { def f(): Int = 1 }")(
      Defn.Given(
        List(Mod.Inline()),
        pname("intOrd"),
        Nil,
        Nil,
        Type.Apply(pname("Ord"), List(pname("Int"))),
        tpl(List(defone))
      )
    )
  }

  test("given-subtype-error".ignore) {
    // it is treaten as alias without '=' sign at the end and {...} is refinement part
    runTestError("given intOrd as _ <: Ord[Int] { def f(): Int = 1 }", "missing = at the end")
  }

  // ---------------------------------
  // GIVEN ALIAS
  // ---------------------------------

  test("given-alias-named") {
    runTestAssert[Stat]("given global as Option[Int] = Some(3)")(
      Defn.GivenAlias(
        Nil,
        pname("global"),
        Nil,
        Nil,
        Type.Apply(pname("Option"), List(pname("Int"))),
        Term.Apply(tname("Some"), List(int(3)))
      )
    )
  }

  test("given-alias-anonymous") {
    runTestAssert[Stat]("given Option[Int] = Some(3)")(
      Defn.GivenAlias(
        Nil,
        anon,
        Nil,
        Nil,
        Type.Apply(pname("Option"), List(pname("Int"))),
        Term.Apply(tname("Some"), List(int(3)))
      )
    )
  }

  test("given-alias-block") {
    runTestAssert[Stat](
      "given global as Option[Int] = { def f(): Int = 1; Some(3) }",
      assertLayout = None
    )(
      Defn.GivenAlias(
        Nil,
        pname("global"),
        Nil,
        Nil,
        Type.Apply(pname("Option"), List(pname("Int"))),
        Term.Block(List(defone, Term.Apply(tname("Some"), List(int(3)))))
      )
    )
  }

  test("given-alias-override-block-error".ignore) {
    runTestError(
      "given global as Option[Int] = { override def f(): Int = 1; Some(3) }",
      "no modifier allowed here"
    )
  }

  test("given-alias-using-named") {
    runTestAssert[Stat]("given ordInt(using ord: Ord[Int]) as Ord[List[Int]] = ???")(
      Defn.GivenAlias(
        Nil,
        pname("ordInt"),
        Nil,
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              tname("ord"),
              Some(Type.Apply(pname("Ord"), List(pname("Int")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(Type.Apply(pname("List"), List(pname("Int"))))),
        tname("???")
      )
    )
  }

  test("given-alias-using-anonymous") {
    runTestAssert[Stat]("given (using ord: Ord[Int]) as Ord[List[Int]] = ???")(
      Defn.GivenAlias(
        Nil,
        anon,
        Nil,
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              tname("ord"),
              Some(Type.Apply(pname("Ord"), List(pname("Int")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(Type.Apply(pname("List"), List(pname("Int"))))),
        tname("???")
      )
    )
  }

  test("given-alias-inline-subtype") {
    runTestAssert[Stat]("inline given intOrd as _ <: Ord[Int] = ???")(
      Defn.GivenAlias(
        List(Mod.Inline()),
        pname("intOrd"),
        Nil,
        Nil,
        Type.Placeholder(Type.Bounds(None, Some(Type.Apply(pname("Ord"), List(pname("Int")))))),
        tname("???")
      )
    )
  }

  test("given-alias-subtype-noinline-error".ignore) {
    runTestError(
      "given intOrd as _ <: Ord[Int] = ???",
      "is only allowed for given with inline modifier"
    )
  }

  test("given-alias-combo") {
    runTestAssert[Stat]("inline given intOrd as _ <: Ord[Int] { val c: String } = ???")(
      Defn.GivenAlias(
        List(Mod.Inline()),
        pname("intOrd"),
        Nil,
        Nil,
        Type.Placeholder(
          Type.Bounds(
            None,
            Some(
              Type.Refine(
                Some(Type.Apply(pname("Ord"), List(pname("Int")))),
                List(Decl.Val(Nil, List(Pat.Var(tname("c"))), pname("String")))
              )
            )
          )
        ),
        tname("???")
      )
    )
  }

  // ---------------------------------
  // USING
  // ---------------------------------

  test("using-named") {
    runTestAssert[Stat]("def f(a: Int)(using ord: UInt): Int = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("ord", "UInt"))),
        Some(pname("Int")),
        tname("???")
      )
    )
  }

  test("using-anonymous") {
    runTestAssert[Stat]("def f(a: Int)(using UInt): Int = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("", "UInt"))),
        Some(pname("Int")),
        tname("???")
      )
    )
  }

  test("using-multiple-parens") {
    runTestAssert[Stat]("def f(a: Int)(using ui: UInt)(using us: UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(
          List(tparam("a", "Int")),
          List(tparamUsing("ui", "UInt")),
          List(tparamUsing("us", "UString"))
        ),
        Some(pname("Boolean")),
        tname("???")
      )
    )
    runTestAssert[Stat]("def f(a: Int)(using UInt)(using UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(
          List(tparam("a", "Int")),
          List(tparamUsing("", "UInt")),
          List(tparamUsing("", "UString"))
        ),
        Some(pname("Boolean")),
        tname("???")
      )
    )
  }

  test("using-many-single-paren") {
    val paramByName =
      Term.Param(List(Mod.Using()), tname("us"), Some(Type.ByName(pname("UString"))), None)
    runTestAssert[Stat]("def f(a: Int)(using ui: UInt, us: => UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("ui", "UInt"), paramByName)),
        Some(pname("Boolean")),
        tname("???")
      )
    )
    runTestAssert[Stat]("def f(a: Int)(using UInt, UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("", "UInt"), tparamUsing("", "UString"))),
        Some(pname("Boolean")),
        tname("???")
      )
    )
  }

  test("using-complex") {
    runTestAssert[Stat]("def f(x: String)(using Int)(y: String)(using b: Int): Unit = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(
          List(tparam("x", "String")),
          List(tparamUsing("", "Int")),
          List(tparam("y", "String")),
          List(tparamUsing("b", "Int"))
        ),
        Some(pname("Unit")),
        tname("???")
      )
    )
  }

  test("using-mix-named-anonymous-error".ignore) {
    runTestError(
      "def f(using a: Int, String): Unit = ???",
      "unable to mix named and anonymous using"
    )
    runTestError(
      "def f(using Int, b: String): Unit = ???",
      "unable to mix named and anonymous using"
    )
  }

  test("using-multiple-using-single-parent-error".ignore) {
    runTestError(
      "def f(using a: Int, using b: String): Unit = ???",
      "using is applied for all parameters inside brackets"
    )
  }

  test("using-added-middle-paren-error".ignore) {
    runTestError(
      "def f(a: Int, using b: String): Unit = ???",
      "using is applied for all parameters inside brackets"
    )
  }

  test("using-named-with-by-name-parameter") {
    val usingByName =
      Term.Param(List(Mod.Using()), tname("a"), Some(Type.ByName(pname("Int"))), None)
    runTestAssert[Stat]("def f(using a: => Int): Unit = ???")(
      Defn.Def(Nil, tname("f"), Nil, List(List(usingByName)), Some(pname("Unit")), tname("???"))
    )
  }

  test("using-call-site") {
    runTestAssert[Stat]("val a = f()(using a)(using 3, true)")(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("a"))),
        None,
        Term.ApplyUsing(
          Term.ApplyUsing(Term.Apply(tname("f"), Nil), List(tname("a"))),
          List(int(3), Lit.Boolean(true))
        )
      )
    )
  }

  // ---------------------------------
  // GIVEN IMPORT
  // ---------------------------------

  test("import-given") {
    runTestAssert[Stat]("import File.{ given _ }")(
      Import(List(Importer(Term.Name("File"), List(Importee.Given(Importee.Wildcard())))))
    )

    runTestAssert[Stat]("import File.{ given TC }")(
      Import(List(Importer(Term.Name("File"), List(Importee.Given(Importee.Name(Name("TC")))))))
    )

    runTestAssert[Stat]("import File.{ given TC, given AC, _ }")(
      Import(
        List(
          Importer(
            Term.Name("File"),
            List(
              Importee.Given(Importee.Name(Name("TC"))),
              Importee.Given(Importee.Name(Name("AC"))),
              Importee.Wildcard()
            )
          )
        )
      )
    )
  }
}
