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
      tplNoBody()
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
      tinfix(tname("x"), "+", int(2))
    ))
  }

  test("val with erased lambda") {
    val code = "val lambdaWithErasedEv: (erased Ev, Int) => Int = (erased ev, x) => x + 2"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("lambdaWithErasedEv")),
      Some(pfunc(Type.FunctionArg(List(Mod.Erased()), pname("Ev")), pname("Int"))(pname("Int"))),
      tfunc(tparam(List(Mod.Erased()), "ev"), tparam("x"))(tinfix(tname("x"), "+", int(2)))
    ))
  }

  test("erased val") {
    val code = "erased val erasedEvidence: Ev = null"
    runTestAssert[Stat](code)(
      Defn.Val(List(Mod.Erased()), List(patvar("erasedEvidence")), Some(pname("Ev")), Lit.Null())
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
      tname("???")
    ))
  }

  test("def with both using and erased") {
    val code = "def turnedOn(using erased ev: IsOff[S]): Machine[On] = new Machine[On]"
    runTestAssert[Stat](code)(Defn.Def(
      Nil,
      tname("turnedOn"),
      Nil,
      List(tparam(List(Mod.Erased(), Mod.Using()), "ev", papply("IsOff", "S")) :: Nil),
      Some(papply("Machine", "On")),
      Term.New(init(papply("Machine", "On")))
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
      Term.New(init(papply("IsEmpty", "Empty")))
    ))
  }

  test("erased param in lambda as argument") {
    val code = """|List(1, 2, 3).map {
                  |  (using erased i: Int) => i
                  |}""".stripMargin
    runTestAssert[Stat](code)(tapply(
      tselect(tapply(tname("List"), int(1), int(2), int(3)), "map"),
      blk(tfunc(tparam(List(Mod.Using(), Mod.Erased()), "i", "Int"))(tname("i")))
    ))
  }

  test("anonymous-method 1") {
    val code = "val fun = (using erased ctx: Context) => ctx.open"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "ctx", "Context"))(tselect("ctx", "open"))
    ))
  }

  test("anonymous-method 2") {
    val code = "val fun = (using erased ctx) => ctx.open"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "ctx"))(tselect("ctx", "open"))
    ))
  }

  test("anonymous-method 3") {
    val code = "val fun = (using erased _: Context) => ctx.open"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using(), Mod.Erased()), "_", "Context"))(tselect("ctx", "open"))
    ))
  }

  test("lambda-method-parameter") {
    val code = """|LazyBody {
                  |  (using erased ctx: Context) => 3
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code)(tapply(
      tname("LazyBody"),
      blk(tfunc(tparam(List(Mod.Using(), Mod.Erased()), "ctx", "Context"))(int(3)))
    ))
  }

  test("dependent-type") {
    val code = "val extractor: (erased e: Entry) => e.Key = extractKey"
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("extractor")),
      Some(pfunc(Type.TypedParam(pname("e"), pname("Entry"), List(Mod.Erased())))(pselect("e", "Key"))),
      tname("extractKey")
    ))
  }

}
