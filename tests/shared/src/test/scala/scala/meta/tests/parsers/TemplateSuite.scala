package scala.meta.tests
package parsers

import scala.meta._, Defn.{Trait, Object, Class}
import scala.meta.dialects.Scala211

class TemplateSuite extends ParseSuite {
  test("trait T") {
    val tree = templStat("trait T")
    assertTree(tree) {
      Trait(Nil, Type.Name("T"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate())
    }
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
      Trait(Nil, Type.Name("T"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate())
    }
  }

  test("trait F[T]") {
    assertTree(templStat("trait F[T]")) {
      Trait(
        Nil,
        Type.Name("F"),
        Type.ParamClause(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
        ),
        EmptyCtor(),
        EmptyTemplate()
      )
    }
  }

  test("trait A extends B") {
    assertTree(templStat("trait A extends B")) {
      Trait(
        Nil,
        Type.Name("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil, EmptySelf(), Nil)
      )
    }
  }

  test("trait Inner <: { val x : Int = 3 }") {
    assertTree(templStat("trait Inner <: { val x : Int = 3 }")) {
      Trait(
        Nil,
        Type.Name("Inner"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          EmptySelf(),
          List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), Some(Type.Name("Int")), Lit.Int(3)))
        )
      )
    }
  }

  test("trait A extends { val x: Int } with B") {
    assertTree(templStat("trait A extends { val x: Int = 2 } with B")) {
      Trait(
        Nil,
        Type.Name("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), Some(Type.Name("Int")), Lit.Int(2)) :: Nil,
          Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil,
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
        Type.Name("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, Self(Term.Name("self"), Some(Type.Name("B"))), Nil)
      )
    }
  }

  test("trait T { def x: Int }") {
    assertTree(templStat("trait T { def x: Int }")) {
      Trait(
        Nil,
        Type.Name("T"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          EmptySelf(),
          List(Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")))
        )
      )
    }
  }

  test("class C") {
    assertTree(templStat("class C")) {
      Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate())
    }
  }

  test("class C[T]") {
    assertTree(templStat("class C[T]")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
        ),
        EmptyCtor(),
        EmptyTemplate()
      )
    }
  }

  test("class A extends B") {
    assertTree(templStat("class A extends B")) {
      Class(
        Nil,
        Type.Name("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil, EmptySelf(), Nil)
      )
    }
  }

  test("class A extends { val x: Int } with B") {
    assertTree(templStat("class A extends { val x: Int = 2 } with B")) {
      Class(
        Nil,
        Type.Name("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), Some(Type.Name("Int")), Lit.Int(2)) :: Nil,
          Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil,
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
        Type.Name("A"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(Nil, Nil, Self(Term.Name("self"), Some(Type.Name("B"))), Nil)
      )
    }
  }

  test("class A { this => }") {
    assertTree(templStat("class A { this => }")) {
      Class(
        Nil,
        Type.Name("A"),
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
        Type.Name("A"),
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
        Type.Name("C"),
        Type.ParamClause(Nil),
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          EmptySelf(),
          List(Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")))
        )
      )
    }
  }

  test("class C(x: Int)") {
    assertTree(templStat("class C(x: Int)")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name.Anonymous(),
          (Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil
        ),
        EmptyTemplate()
      )
    }

  }

  test("class C private(x: Int)") {
    assertTree(templStat("class C private(x: Int)")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Mod.Private(Name.Anonymous()) :: Nil,
          Name.Anonymous(),
          (Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil
        ),
        EmptyTemplate()
      )
    }

  }

  test("class C(val x: Int)") {
    assertTree(templStat("class C(val x: Int)")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name.Anonymous(),
          (Term.Param(Mod.ValParam() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil)
            :: Nil
        ),
        EmptyTemplate()
      )
    }

  }

  test("class C(var x: Int)") {
    assertTree(templStat("class C(var x: Int)")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name.Anonymous(),
          (Term.Param(Mod.VarParam() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil)
            :: Nil
        ),
        EmptyTemplate()
      )
    }

  }

  test("class C(implicit x: Int)") {
    assertTree(templStat("class C(implicit x: Int)")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name.Anonymous(),
          (Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil)
            :: Nil
        ),
        EmptyTemplate()
      )
    }

  }

  test("class C(override val x: Int)") {
    assertTree(templStat("class C(override val x: Int)")) {
      Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name.Anonymous(),
          (Term.Param(
            List(Mod.Override(), Mod.ValParam()),
            Term.Name("x"),
            Some(Type.Name("Int")),
            None
          ) :: Nil) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("case class C(x: Int)(y: => Int)") {
    assertTree(templStat("case class C(x: Int)(y: => Int)")) {
      Class(
        Mod.Case() :: Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name.Anonymous(),
          (Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) ::
            (Term.Param(Nil, Term.Name("y"), Some(Type.ByName(Type.Name("Int"))), None) :: Nil)
            :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("object O") {
    val Object(Nil, Term.Name("O"), EmptyTemplate()) = templStat("object O")
  }

  test("case object O") {
    val Object(Mod.Case() :: Nil, Term.Name("O"), EmptyTemplate()) = templStat("case object O")
  }

  test("object A extends B") {
    val Object(
      Nil,
      Term.Name("A"),
      Template(Nil, Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil, EmptySelf(), Nil)
    ) =
      templStat("object A extends B")
  }

  test("object A extends { val x: Int } with B") {
    val Object(
      Nil,
      Term.Name("A"),
      Template(
        Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), Some(Type.Name("Int")), Lit(2)) :: Nil,
        Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil,
        EmptySelf(),
        Nil
      )
    ) =
      templStat("object A extends { val x: Int = 2 } with B")
  }

  test("object A extends { self: B => }") {
    val Object(
      Nil,
      Term.Name("A"),
      Template(Nil, Nil, Self(Term.Name("self"), Some(Type.Name("B"))), Nil)
    ) =
      templStat("object A { self: B => }")
  }

  test("trait B extends A.type") {
    val err = intercept[ParseException] {
      templStat("trait B extends A.type")
    }
    assertNoDiff(err.shortMessage, "class type required but A.type found")
  }

  test("#3142: template with anonymous self type") {
    val code = "class foo { _: Int => }"
    val tree = Defn.Class(
      Nil,
      Type.Name("foo"),
      Nil,
      Ctor.Primary(Nil, Name(""), Nil),
      Template(Nil, Nil, Self(Name.Placeholder(), Some(Type.Name("Int"))), Nil, Nil)
    )
    checkStat(code, code)(tree)
  }

  test("blank after template 1") {
    val code =
      """|class DerivationSpec {
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
    val layout =
      """|class DerivationSpec {
         |  case class Foo()
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
        List(
          Defn.Class(
            List(Mod.Case()),
            pname("Foo"),
            Nil,
            ctorp(Nil),
            tpl(Nil)
          ),
          Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo"))))),
          Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo")))))
        )
      )
    )
    // code parses as specified tree
    checkStat(code, layout)(tree)
    // however, the parsed layout itself has different syntax (and, hence, tree)
    assertNotEquals(parseStat(layout, implicitly[Dialect]).reprint, layout)
  }

  test("blank after template 2") {
    val code =
      """|class DerivationSpec {
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
    val layout =
      """|class DerivationSpec {
         |  case class Foo() extends Bar
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
        List(
          Defn.Class(
            List(Mod.Case()),
            pname("Foo"),
            Nil,
            ctorp(Nil),
            tpl(List(init("Bar")), Nil)
          ),
          Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo"))))),
          Term.Block(List(Term.ApplyType(tname("deriveEncoder"), List(pname("Foo")))))
        )
      )
    )
    // code parses as specified tree
    checkStat(code, layout)(tree)
    // however, the parsed layout itself has different syntax (and, hence, tree)
    assertNotEquals(parseStat(layout, implicitly[Dialect]).reprint, layout)
  }

}
