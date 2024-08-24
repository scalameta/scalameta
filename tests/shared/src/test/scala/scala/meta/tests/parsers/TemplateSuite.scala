package scala.meta.tests
package parsers

import scala.meta._

class TemplateSuite extends ParseSuite {
  import Defn.Class
  import Defn.Object
  import Defn.Trait

  implicit val dialect: Dialect = dialects.Scala211

  test("trait T") {
    val tree = templStat("trait T")
    assertTree(tree)(Trait(Nil, pname("T"), Nil, ctor, tplNoBody()))
    def testTokens(t: Tree): Unit = {
      interceptMessage[NoSuchElementException]("token 0 out of 0")(t.tokens.head)
      interceptMessage[NoSuchElementException]("token -1 out of 0")(t.tokens.last)
    }
    val traitTree = tree.asInstanceOf[Trait]
    testTokens(traitTree.tparamClause)
    testTokens(traitTree.ctor)
    testTokens(traitTree.templ)
  }

  test("trait T {}") {
    assertTree(templStat("trait T {}")) {
      Trait(Nil, pname("T"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate())
    }
  }

  test("trait F[T]") {
    assertTree(templStat("trait F[T]")) {
      Trait(Nil, pname("F"), pparam("T") :: Nil, ctor, tplNoBody())
    }
  }

  test("trait A extends B") {
    assertTree(templStat("trait A extends B")) {
      Trait(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Init(pname("B"), anon, emptyArgClause) :: Nil, EmptySelf(), Nil)
      )
    }
  }

  test("trait Inner <: { val x : Int = 3 }") {
    assertTree(templStat("trait Inner <: { val x : Int = 3 }")) {
      Trait(
        Nil,
        pname("Inner"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          EmptySelf(),
          List(Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), int(3)))
        )
      )
    }
  }

  test("trait A extends { val x: Int } with B") {
    assertTree(templStat("trait A extends { val x: Int = 2 } with B")) {
      Trait(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), int(2)) :: Nil,
          Init(pname("B"), anon, emptyArgClause) :: Nil,
          EmptySelf(),
          Nil
        )
      )
    }
  }

  test("trait A extends { self: B => }") {
    assertTree(templStat("trait A { self: B => }")) {
      Trait(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, self("self", "B"), Nil)
      )
    }
  }

  test("trait T { def x: Int }") {
    assertTree(templStat("trait T { def x: Int }")) {
      Trait(
        Nil,
        pname("T"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, EmptySelf(), List(Decl.Def(Nil, tname("x"), Nil, Nil, pname("Int"))))
      )
    }
  }

  test("class C") {
    assertTree(templStat("class C"))(Class(Nil, pname("C"), Nil, ctor, tplNoBody()))
  }

  test("class C[T]") {
    assertTree(templStat("class C[T]")) {
      Class(Nil, pname("C"), pparam("T") :: Nil, ctor, tplNoBody())
    }
  }

  test("class A extends B") {
    assertTree(templStat("class A extends B")) {
      Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Init(pname("B"), anon, emptyArgClause) :: Nil, EmptySelf(), Nil)
      )
    }
  }

  test("class A extends { val x: Int } with B") {
    assertTree(templStat("class A extends { val x: Int = 2 } with B")) {
      Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), int(2)) :: Nil,
          Init(pname("B"), anon, emptyArgClause) :: Nil,
          EmptySelf(),
          Nil
        )
      )
    }
  }

  test("class A extends { self: B => }") {
    assertTree(templStat("class A { self: B => }")) {
      Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, self("self", "B"), Nil)
      )
    }
  }

  test("class A { this => }") {
    assertTree(templStat("class A { this => }")) {
      Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, Self(Name.This(), None), Nil)
      )
    }
  }

  test("class A { _ => }") {
    assertTree(templStat("class A { this => }")) {
      Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, Self(Name.This(), None), Nil)
      )
    }
  }

  test("class C { def x: Int }") {
    assertTree(templStat("class C { def x: Int }")) {
      Class(
        Nil,
        pname("C"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, EmptySelf(), List(Decl.Def(Nil, tname("x"), Nil, Nil, pname("Int"))))
      )
    }
  }

  test("class C(x: Int)") {
    assertTree(templStat("class C(x: Int)")) {
      Class(Nil, pname("C"), Nil, ctorp(tparam("x", "Int")), tplNoBody())
    }

  }

  test("class C private(x: Int)") {
    assertTree(templStat("class C private(x: Int)")) {
      Class(
        Nil,
        pname("C"),
        Nil,
        Ctor.Primary(Mod.Private(anon) :: Nil, anon, (tparam("x", "Int") :: Nil) :: Nil),
        tplNoBody()
      )
    }

  }

  test("class C(val x: Int)") {
    assertTree(templStat("class C(val x: Int)")) {
      Class(Nil, pname("C"), Nil, ctorp(tparam(Mod.ValParam() :: Nil, "x", "Int")), tplNoBody())
    }

  }

  test("class C(var x: Int)") {
    assertTree(templStat("class C(var x: Int)")) {
      Class(Nil, pname("C"), Nil, ctorp(tparam(Mod.VarParam() :: Nil, "x", "Int")), tplNoBody())
    }

  }

  test("class C(implicit x: Int)") {
    assertTree(templStat("class C(implicit x: Int)")) {
      Class(Nil, pname("C"), Nil, ctorp(tparam(Mod.Implicit() :: Nil, "x", "Int")), tplNoBody())
    }

  }

  test("class C(override val x: Int)") {
    assertTree(templStat("class C(override val x: Int)")) {
      Class(
        Nil,
        pname("C"),
        Nil,
        ctorp(tparam(List(Mod.Override(), Mod.ValParam()), "x", "Int")),
        tplNoBody()
      )
    }
  }

  test("case class C(x: Int)(y: => Int)") {
    assertTree(templStat("case class C(x: Int)(y: => Int)")) {
      Class(
        Mod.Case() :: Nil,
        pname("C"),
        Nil,
        ctorp(tparam("x", "Int") :: Nil, tparam("y", Type.ByName(pname("Int"))) :: Nil),
        tplNoBody()
      )
    }
  }

  test("object O")(assertTree(templStat("object O"))(Object(Nil, tname("O"), tplNoBody())))

  test("case object O") {
    assertTree(templStat("case object O"))(Object(Mod.Case() :: Nil, tname("O"), tplNoBody()))
  }

  test("object A extends B") {
    assertTree(templStat("object A extends B"))(Object(
      Nil,
      tname("A"),
      Template(Nil, Init(pname("B"), anon, emptyArgClause) :: Nil, EmptySelf(), Nil)
    ))
  }

  test("object A extends { val x: Int } with B") {
    assertTree(templStat("object A extends { val x: Int = 2 } with B"))(Object(
      Nil,
      tname("A"),
      Template(
        Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), int(2)) :: Nil,
        Init(pname("B"), anon, emptyArgClause) :: Nil,
        EmptySelf(),
        Nil
      )
    ))
  }

  test("object A extends { self: B => }") {
    assertTree(templStat("object A { self: B => }"))(
      Object(Nil, tname("A"), Template(Nil, Nil, self("self", "B"), Nil))
    )
  }

  test("trait B extends A.type") {
    val err = intercept[ParseException](templStat("trait B extends A.type"))
    assertNoDiff(err.shortMessage, "class type required but A.type found")
  }

  test("#3142: template with anonymous self type") {
    val code = "class foo { _: Int => }"
    val tree = Defn.Class(
      Nil,
      pname("foo"),
      Nil,
      EmptyCtor(),
      Template(Nil, Nil, Self(Name.Placeholder(), Some(pname("Int"))), Nil, Nil)
    )
    checkStat(code, code)(tree)
  }

  test("blank after template 1") {
    val code = """|class DerivationSpec {
                  |  case class Foo()
                  |
                  |  {
                  |    deriveEncoder[Foo]
                  |  }
                  |
                  |  {
                  |    deriveEncoder[Foo]
                  |  }
                  |}
                  |""".stripMargin
    val layout = """|class DerivationSpec {
                    |  case class Foo()
                    |
                    |  {
                    |    deriveEncoder[Foo]
                    |  }
                    |  {
                    |    deriveEncoder[Foo]
                    |  }
                    |}
                    |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("DerivationSpec"),
      Nil,
      ctor,
      tpl(
        Defn.Class(List(Mod.Case()), pname("Foo"), Nil, ctorp(Nil), tplNoBody()),
        Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo"))))),
        Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo")))))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("blank after template 2") {
    val code = """|class DerivationSpec {
                  |  case class Foo() extends Bar
                  |
                  |  {
                  |    deriveEncoder[Foo]
                  |  }
                  |
                  |  {
                  |    deriveEncoder[Foo]
                  |  }
                  |}
                  |""".stripMargin
    val layout = """|class DerivationSpec {
                    |  case class Foo() extends Bar
                    |
                    |  {
                    |    deriveEncoder[Foo]
                    |  }
                    |  {
                    |    deriveEncoder[Foo]
                    |  }
                    |}
                    |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("DerivationSpec"),
      Nil,
      ctor,
      tpl(
        Defn.Class(List(Mod.Case()), pname("Foo"), Nil, ctorp(), tplNoBody(init("Bar"))),
        Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo"))))),
        Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo")))))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

}
