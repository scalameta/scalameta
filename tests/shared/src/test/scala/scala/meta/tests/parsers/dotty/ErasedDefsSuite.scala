package scala.meta.tests.parsers.dotty

import scala.meta._

class ErasedDefsSuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/experimental/erased-defs.html]]
   */

  test("def with erased params") {
    val code = "def methodWithErasedEv(erased ev: Ev, x: Int): Int = x + 2"
    runTestAssert[Stat](code)(
      Defn.Def(
        Nil,
        tname("methodWithErasedEv"),
        Nil,
        List(List(tparam(List(Mod.Erased()), "ev", "Ev"), tparam("x", "Int"))),
        Some(pname("Int")),
        Term.ApplyInfix(tname("x"), tname("+"), Nil, List(Lit.Int(2)))
      )
    )
  }

  test("val with erased lambda") {
    val code = "val lambdaWithErasedEv: (erased Ev, Int) => Int = (erased ev, x) => x + 2"
    runTestAssert[Stat](code)(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("lambdaWithErasedEv"))),
        Some(
          Type.Function(
            List(Type.FunctionArg(List(Mod.Erased()), pname("Ev")), pname("Int")),
            pname("Int")
          )
        ),
        Term.Function(
          List(tparam(List(Mod.Erased()), "ev"), tparam(Nil, "x")),
          Term.ApplyInfix(tname("x"), tname("+"), Nil, List(int(2)))
        )
      )
    )
  }

  test("erased val") {
    val code = "erased val erasedEvidence: Ev = null"
    runTestAssert[Stat](code)(
      Defn.Val(
        List(Mod.Erased()),
        List(Pat.Var(tname("erasedEvidence"))),
        Some(pname("Ev")),
        Lit.Null()
      )
    )
  }

  test("def with only one erased param") {
    val code = "def methodWithErasedEv(x: Int, erased ev: Ev): Int = ???"
    runTestAssert[Stat](code)(
      Defn.Def(
        Nil,
        tname("methodWithErasedEv"),
        Nil,
        List(List(tparam("x", "Int"), tparam(List(Mod.Erased()), "ev", "Ev"))),
        Some(pname("Int")),
        tname("???")
      )
    )
  }

  test("def with both using and erased") {
    val code = "def turnedOn(using erased ev: IsOff[S]): Machine[On] = new Machine[On]"
    runTestAssert[Stat](code)(
      Defn.Def(
        Nil,
        tname("turnedOn"),
        Nil,
        List(
          tparam(
            List(Mod.Erased(), Mod.Using()),
            "ev",
            Type.Apply(pname("IsOff"), List(pname("S")))
          ) :: Nil
        ),
        Some(Type.Apply(pname("Machine"), List(pname("On")))),
        Term.New(Init(Type.Apply(pname("Machine"), List(pname("On"))), anon, emptyArgClause))
      )
    )
  }

  test("erased class") {
    val code = "erased class CanRead"
    runTestAssert[Stat](code)(
      Defn.Class(List(Mod.Erased()), pname("CanRead"), Nil, ctor, tpl(Nil))
    )
  }

  test("erased given") {
    val code = "erased given IsEmpty[Empty] = new IsEmpty[Empty]"
    runTestAssert[Stat](code)(
      Defn.GivenAlias(
        List(Mod.Erased()),
        anon,
        None,
        Type.Apply(pname("IsEmpty"), List(pname("Empty"))),
        Term.New(Init(Type.Apply(pname("IsEmpty"), List(pname("Empty"))), anon, emptyArgClause))
      )
    )
  }

  test("erased param in lambda as argument") {
    val code =
      """|List(1, 2, 3).map {
         |  (using erased i: Int) => i
         |}""".stripMargin
    val layout = "List(1, 2, 3).map { (using erased i: Int) => i }"
    runTestAssert[Stat](code, layout)(
      Term.Apply(
        Term.Select(
          Term.Apply(tname("List"), List(int(1), int(2), int(3))),
          tname("map")
        ),
        Term.Function(
          List(tparam(List(Mod.Using(), Mod.Erased()), "i", "Int")),
          tname("i")
        ) :: Nil
      )
    )
  }

  test("anonymous-method 1") {
    val code = "val fun = (using erased ctx: Context) => ctx.open"
    runTestAssert[Stat](code)(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("fun"))),
        None,
        Term.Function(
          List(tparam(List(Mod.Using(), Mod.Erased()), "ctx", "Context")),
          Term.Select(tname("ctx"), tname("open"))
        )
      )
    )
  }

  test("anonymous-method 2") {
    val code = "val fun = (using erased ctx) => ctx.open"
    runTestAssert[Stat](code)(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("fun"))),
        None,
        Term.Function(
          List(tparam(List(Mod.Using(), Mod.Erased()), "ctx")),
          Term.Select(tname("ctx"), tname("open"))
        )
      )
    )
  }

  test("anonymous-method 3") {
    val code = "val fun = (using erased _: Context) => ctx.open"
    runTestAssert[Stat](code)(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("fun"))),
        None,
        Term.Function(
          List(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context")),
          Term.Select(tname("ctx"), tname("open"))
        )
      )
    )
  }

  test("lambda-method-parameter") {
    val code =
      """|LazyBody {
         |  (using erased ctx: Context) => 3
         |}
         |""".stripMargin
    val layout = "LazyBody { (using erased ctx: Context) => 3 }"
    runTestAssert[Stat](code, layout)(
      Term.Apply(
        tname("LazyBody"),
        Term.Function(
          List(tparam(List(Mod.Using(), Mod.Erased()), "ctx", "Context")),
          Lit.Int(3)
        ) :: Nil
      )
    )
  }

  test("dependent-type") {
    val code = "val extractor: (erased e: Entry) => e.Key = extractKey"
    runTestAssert[Stat](code)(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("extractor"))),
        Some(
          Type.Function(
            List(Type.TypedParam(Type.Name("e"), Type.Name("Entry"), List(Mod.Erased()))),
            Type.Select(Term.Name("e"), Type.Name("Key"))
          )
        ),
        Term.Name("extractKey")
      )
    )
  }

}
