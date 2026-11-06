package scala.meta.tests.parsers.dotty

import scala.meta._

class ErasedDefsSuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/experimental/erased-defs.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/experimental/erased-defs-spec.html]]
   */

  test("class with erased params") {
    val code = "class ClassWithErasedEv(erased ev: Ev, x: Int) {}"
    val layout = "class ClassWithErasedEv(erased ev: Ev, x: Int)"
    val tree = Defn.Class(
      Nil,
      pname("ClassWithErasedEv"),
      Nil,
      ctorp(tparam(List(Mod.Erased()), "ev", "Ev"), tparam("x", "Int")),
      tplNoBody(),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("def with erased params") {
    val code = "def methodWithErasedEv(erased ev: Ev, x: Int): Int = x + 2"
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      tname("methodWithErasedEv"),
      Nil,
      List(List(tparam(List(Mod.Erased()), "ev", "Ev"), tparam("x", "Int"))),
      Some(pname("Int")),
      tinfix(tname("x"), "+", int(2)),
    ))
  }

  test("val with erased lambda") {
    val code = "val lambdaWithErasedEv: (erased Ev, Int) => Int = (erased ev, x) => x + 2"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("lambdaWithErasedEv")),
      Some(pfunc(Type.FunctionArg(List(Mod.Erased()), pname("Ev")), pname("Int"))(pname("Int"))),
      tfunc(tparam(List(Mod.Erased()), "ev"), tparam("x"))(tinfix(tname("x"), "+", int(2))),
    ))
  }

  test("erased val") {
    val code = "erased val erasedEvidence: Ev = null"
    runTestAssert[Stat](code)(
      Defn.Val(List(Mod.Erased()), List(patvar("erasedEvidence")), Some(pname("Ev")), Lit.Null()),
    )
  }

  test("def with only one erased param") {
    val code = "def methodWithErasedEv(x: Int, erased ev: Ev): Int = ???"
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      tname("methodWithErasedEv"),
      Nil,
      List(List(tparam("x", "Int"), tparam(List(Mod.Erased()), "ev", "Ev"))),
      Some(pname("Int")),
      tname("???"),
    ))
  }

  test("def with both using and erased") {
    val code = "def turnedOn(using erased ev: IsOff[S]): Machine[On] = new Machine[On]"
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      tname("turnedOn"),
      Nil,
      List(tparam(List(Mod.Using(), Mod.Erased()), "ev", papply("IsOff", "S")) :: Nil),
      Some(papply("Machine", "On")),
      Term.New(init(papply("Machine", "On"))),
    ))
  }

  test("erased class") {
    val code = "erased class CanRead"
    runTestAssert[Stat](code)(Defn.Class(List(Mod.Erased()), pname("CanRead"), Nil, ctor, tplNoBody()))
  }

  test("erased given") {
    val code = "erased given IsEmpty[Empty] = new IsEmpty[Empty]"
    runTestAssert[Stat](code)(Defn.GivenAlias(
      List(Mod.Erased()),
      anon,
      None,
      papply("IsEmpty", "Empty"),
      Term.New(init(papply("IsEmpty", "Empty"))),
    ))
  }

  test("erased param in lambda as argument") {
    val code =
      """|List(1, 2, 3).map {
         |  (using erased i: Int) => i
         |}""".stripMargin
    runTestAssert[Stat](code)(tapply(
      tselect(tapply(tname("List"), int(1), int(2), int(3)), "map"),
      blk(tfunc(tparam(List(Mod.Using(), Mod.Erased()), "i", "Int"))(tname("i"))),
    ))
  }

  test("anonymous-method 1") {
    val code = "val fun = (using erased ctx: Context) => ctx.open"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "ctx", "Context"))(tselect("ctx", "open")),
    ))
  }

  test("anonymous-method 2") {
    val code = "val fun = (using erased ctx) => ctx.open"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "ctx"))(tselect("ctx", "open")),
    ))
  }

  test("anonymous-method 3") {
    val code = "val fun = (using erased _: Context) => ctx.open"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"))(tselect("ctx", "open")),
    ))
  }

  test("anonymous-method: using erased placeholder without type") {
    val code = "val fun = (using erased _) => ctx.open"
    val tree = Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_"))(tselect("ctx", "open")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("anonymous-method: using placeholder") {
    val code = "val fun = (using _: Context) => ctx.open"
    val tree = Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using()), "_", "Context"))(tselect("ctx", "open")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("anonymous-method: erased placeholder") {
    val code = "val fun = (erased _: Context) => ctx.open"
    val tree = Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Erased()), "_", "Context"))(tselect("ctx", "open")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("anonymous-method: using erased placeholder, then a param") {
    val code = "val fun = (using erased _: Context, x: Int) => x"
    val tree = Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"), tparam("x", "Int"))(tname("x")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("anonymous-method: erased param, then a placeholder") {
    val code = "val fun = (erased x: Context, _: Int) => x"
    val tree = Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Erased()), "x", "Context"), tparam("_", "Int"))(tname("x")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("context function: using erased placeholder") {
    val code = "val fun = (using erased _: Context) ?=> ctx.open"
    val tree = Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tctxfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"))(tselect("ctx", "open")),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("lambda argument in braces: using erased placeholder") {
    val code =
      """|xs.map {
         |  (using erased _: Context) => 1
         |}""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"))(int(1))),
    )
    runTestAssert[Stat](code)(tree)
  }

  test("using erased placeholder in parens") {
    val code = "(using erased _: Context)"
    val tree = Term.AnonymousFunction(
      Term.Ascribe(tinfix(tname("using"), "erased", Term.Placeholder()), pname("Context")),
    )
    runTestAssert[Stat](code, assertLayout = None)(tree)
  }

  test("using erased placeholder without type in parens") {
    val code = "(using erased _)"
    val tree = Term.AnonymousFunction(tinfix(tname("using"), "erased", Term.Placeholder()))
    runTestAssert[Stat](code, assertLayout = None)(tree)
  }

  test("lambda argument in parens: using erased placeholder") {
    val code = "xs.map((using erased _: Context) => 1)"
    val tree = tapply(
      tselect("xs", "map"),
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"))(int(1)),
    )
    parseAndCheckTree[Stat](code, "xs.map { (using erased _: Context) => 1 }")(tree)
  }

  test("lambda argument after colon: using erased placeholder") {
    val code = "xs.map: (using erased _: Context) => 1"
    val layout =
      """|xs.map {
         |  (using erased _: Context) => 1
         |}
         |""".stripMargin
    val tree = tapply(
      tselect("xs", "map"),
      blk(Term.Function(
        Term.ParamClause(
          List(Term.Param(
            List(Mod.Using(), Mod.Erased()),
            Name.Placeholder(),
            Some(pname("Context")),
            None,
          )),
          Some(Mod.Using()),
        ),
        lit(1),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("anonymous-method: using erased placeholder in double parens") {
    val code = "((using erased _: Context)) => 1"
    val tree = tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"))(int(1))
    runTestAssert[Stat](code, assertLayout = None)(tree)
  }

  test("lambda-method-parameter") {
    val code =
      """|LazyBody {
         |  (using erased ctx: Context) => 3
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(tapply(
      tname("LazyBody"),
      blk(tfunc(tparam(List(Mod.Using(), Mod.Erased()), "ctx", "Context"))(int(3))),
    ))
  }

  test("dependent-type") {
    val code = "val extractor: (erased e: Entry) => e.Key = extractKey"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("extractor")),
      Some(pfunc(Type.TypedParam(pname("e"), pname("Entry"), List(Mod.Erased())))(pselect("e", "Key"))),
      tname("extractKey"),
    ))
  }

}
