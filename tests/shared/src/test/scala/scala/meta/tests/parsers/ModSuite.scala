package scala.meta.tests
package parsers

import scala.meta._

class ModSuite extends ParseSuite {
  test("implicit") {
    assertTree(templStat("implicit object A"))(
      Defn.Object(List(Mod.Implicit()), tname("A"), tplNoBody())
    )
    assertTree(templStat("implicit class A"))(
      Defn.Class(List(Mod.Implicit()), pname("A"), Nil, ctor, tplNoBody())
    )
    assertTree(templStat("implicit case object A"))(
      Defn.Object(List(Mod.Implicit(), Mod.Case()), tname("A"), tplNoBody())
    )

    assertTree(templStat("case class A(implicit val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(Mod.Implicit(), tparam(List(Mod.Implicit(), Mod.ValParam()), "a", "Int")),
        tplNoBody()
      )
    }
    assertTree(templStat("case class A(implicit var a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(Mod.Implicit(), tparam(List(Mod.Implicit(), Mod.VarParam()), "a", "Int")),
        tplNoBody()
      )
    }

    assertTree(templStat("def foo(implicit a: Int): Int = a")) {
      Defn.Def(
        Nil,
        tname("foo"),
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(List(tparam(List(Mod.Implicit()), "a", "Int")), Some(Mod.Implicit())) ::
            Nil
        ) :: Nil,
        Some(pname("Int")),
        tname("a")
      )
    }

    assertTree(templStat("implicit def foo(a: Int): Int = a")) {
      Defn.Def(
        List(Mod.Implicit()),
        tname("foo"),
        Member.ParamClauseGroup(Type.ParamClause(Nil), List(tparam("a", "Int")) :: Nil) :: Nil,
        Some(pname("Int")),
        tname("a")
      )
    }

    assertTree(templStat("implicit val a: Int = 1")) {
      Defn.Val(List(Mod.Implicit()), List(Pat.Var(tname("a"))), Some(pname("Int")), int(1))
    }

    assertTree(templStat("implicit val a: Int")) {
      Decl.Val(List(Mod.Implicit()), List(Pat.Var(tname("a"))), pname("Int"))
    }

    assertTree(templStat("implicit var a: Int = 1")) {
      Defn.Var(List(Mod.Implicit()), List(Pat.Var(tname("a"))), Some(pname("Int")), Some(int(1)))

    }

    assertTree(templStat("implicit var a: Int")) {
      Decl.Var(List(Mod.Implicit()), List(Pat.Var(tname("a"))), pname("Int"))
    }
  }

  test("final") {
    matchSubStructure[Stat]("final object A", { case Defn.Object(List(Mod.Final()), _, _) => () })
    matchSubStructure[Stat](
      "final class A",
      { case Defn.Class(List(Mod.Final()), _, _, _, _) => () }
    )
    matchSubStructure[Stat](
      "final case class A(a: Int)",
      { case Defn.Class(List(Mod.Final(), Mod.Case()), _, _, _, _) => () }
    )
    matchSubStructure[Stat](
      "final case object A",
      { case Defn.Object(List(Mod.Final(), Mod.Case()), _, _) => () }
    )

    assertTree(templStat("case class A(final val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(tparam(List(Mod.Final(), Mod.ValParam()), "a", "Int")),
        tplNoBody()
      )
    }

    matchSubStructure[Stat](
      "final def foo(a: Int): Int = a",
      { case Defn.Def(List(Mod.Final()), _, _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "final val a: Int = 1",
      { case Defn.Val(List(Mod.Final()), _, _, _) => () }
    )

    matchSubStructure[Stat]("final val a: Int", { case Decl.Val(List(Mod.Final()), _, _) => () })

    matchSubStructure[Stat](
      "final var a: Int = 1",
      { case Defn.Var(List(Mod.Final()), _, _, _) => () }
    )

    matchSubStructure[Stat]("final var a: Int", { case Decl.Var(List(Mod.Final()), _, _) => () })

    matchSubStructure[Stat](
      "final type A = Int",
      { case Defn.Type(List(Mod.Final()), _, _, _) => () }
    )
  }

  testParseErrors("def foo(final val a: Int): Int = a")

  test("sealed") {
    matchSubStructure[Stat](
      "sealed trait A",
      { case Defn.Trait(List(Mod.Sealed()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "sealed class A",
      { case Defn.Class(List(Mod.Sealed()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "sealed abstract class A",
      { case Defn.Class(List(Mod.Sealed(), Mod.Abstract()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "sealed case class A(a: Int)",
      { case Defn.Class(List(Mod.Sealed(), Mod.Case()), _, _, _, _) => () }
    )
  }

  testParseErrors("def foo(sealed val a: Int): Int = a")

  test("override") {
    matchSubStructure[Stat](
      "override object A",
      { case Defn.Object(List(Mod.Override()), _, _) => () }
    )

    matchSubStructure[Stat](
      "override case object A",
      { case Defn.Object(List(Mod.Override(), Mod.Case()), _, _) => () }
    )

    matchSubStructure[Stat](
      "override def foo(a: Int): Int = a",
      { case Defn.Def(List(Mod.Override()), _, _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "override val a: Int = 1",
      { case Defn.Val(List(Mod.Override()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "override var a: Int = 1",
      { case Defn.Var(List(Mod.Override()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "override type A = Int",
      { case Defn.Type(List(Mod.Override()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "override def foo(a: Int): Int",
      { case Decl.Def(List(Mod.Override()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "override val a: Int",
      { case Decl.Val(List(Mod.Override()), _, _) => () }
    )

    matchSubStructure[Stat](
      "override var a: Int",
      { case Decl.Var(List(Mod.Override()), _, _) => () }
    )

    matchSubStructure[Stat](
      "override type A",
      { case Decl.Type(List(Mod.Override()), _, _, _) => () }
    )
  }

  testParseErrors("def foo(override val a: Int): Int = a")

  test("case") {
    matchSubStructure[Stat]("case object A", { case Defn.Object(List(Mod.Case()), _, _) => () })

    matchSubStructure[Stat](
      "case class A(a: Int)",
      { case Defn.Class(List(Mod.Case()), _, _, _, _) => () }
    )
  }

  testParseErrors(
    "case case var a: Int",
    "case case val a: Int",
    "case case var a: Int = 1",
    "case case val a: Int = 1",
    "case case class A",
    "case case object A",
    "case case trait A",
    "case case case class A(a: Int)",
    "case case type A",
    "case val a: Int",
    "case var a: Int",
    "case val a: Int = 1",
    "case var a: Int = 1",
    "case def foo(a: Int): Int",
    "case type A",
    "case type A = Int",
    "def foo(case val a: Int): Int = a",
    "case def foo(val a: Int): Int = a",
    "class A(case a: Int)"
  )

  test("abstract") {
    matchSubStructure[Stat](
      "abstract trait A",
      { case Defn.Trait(List(Mod.Abstract()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract class A",
      { case Defn.Class(List(Mod.Abstract()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract case class A(a: Int)",
      { case Defn.Class(List(Mod.Abstract(), Mod.Case()), _, _, _, _) => () }
    )
  }

  testParseErrors("def foo(abstract val a: Int): Int = a", "abstract def foo(val a: Int): Int = a")

  test("lazy") {
    matchSubStructure[Stat](
      "lazy val a: Int = 1",
      { case Defn.Val(List(Mod.Lazy()), _, _, _) => () }
    )
  }

  testParseErrors("def foo(lazy val a: Int): Int = a", "lazy def foo(val a: Int): Int = a")

  test("abstract override") {
    /* Non-trait members modified by `abstract override` receive a typechecking error */
    matchSubStructure[Stat](
      "abstract override object A",
      { case Defn.Object(List(Mod.Abstract(), Mod.Override()), _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override case object A",
      { case Defn.Object(List(Mod.Abstract(), Mod.Override(), Mod.Case()), _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override def foo(a: Int): Int = a",
      { case Defn.Def(List(Mod.Abstract(), Mod.Override()), _, _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override val a: Int = 1",
      { case Defn.Val(List(Mod.Abstract(), Mod.Override()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override var a: Int = 1",
      { case Defn.Var(List(Mod.Abstract(), Mod.Override()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override type A = Int",
      { case Defn.Type(List(Mod.Abstract(), Mod.Override()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override def foo(a: Int): Int",
      { case Decl.Def(List(Mod.Abstract(), Mod.Override()), _, _, _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override val a: Int",
      { case Decl.Val(List(Mod.Abstract(), Mod.Override()), _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override var a: Int",
      { case Decl.Var(List(Mod.Abstract(), Mod.Override()), _, _) => () }
    )

    matchSubStructure[Stat](
      "abstract override type A",
      { case Decl.Type(List(Mod.Abstract(), Mod.Override()), _, _, _) => () }
    )
  }

  testParseErrors("def foo(abstract override val a: Int): Int = a")

  test("covariant in case class") {
    assertTree(templStat("case class A[+T](t: T)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        pparam(List(Mod.Covariant()), "T") :: Nil,
        ctorp(tparam("t", "T")),
        tplNoBody()
      )
    }
  }

  test("covariant in class") {
    assertTree(templStat("class A[+T](t: T)")) {
      Defn.Class(
        Nil,
        pname("A"),
        pparam(List(Mod.Covariant()), "T") :: Nil,
        ctorp(tparam("t", "T")),
        tplNoBody()
      )
    }
  }

  test("covariant in type") {
    assertTree(templStat("type A[+T] = B[T]")) {
      Defn.Type(
        Nil,
        pname("A"),
        pparam(List(Mod.Covariant()), "T") :: Nil,
        Type.Apply(pname("B"), List(pname("T"))),
        noBounds
      )
    }
  }

  test("covariant-like in type") {
    val error = """|<input>:1: error: `]` expected but `identifier` found
                   |type A[`+`T] = B[T]
                   |          ^""".stripMargin
    runTestError[Stat]("type A[`+`T] = B[T]", error)
  }

  test("covariant in def")(interceptParseError("def foo[+T](t: T): Int"))

  test("contravariant in case class") {
    assertTree(templStat("case class A[-T](t: T)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        pparam(List(Mod.Contravariant()), "T") :: Nil,
        ctorp(tparam("t", "T")),
        tplNoBody()
      )
    }
  }

  test("contravariant in class") {
    assertTree(templStat("class A[-T](t: T)")) {
      Defn.Class(
        Nil,
        pname("A"),
        pparam(List(Mod.Contravariant()), "T") :: Nil,
        ctorp(tparam("t", "T")),
        tplNoBody()
      )
    }
  }

  test("contravariant-like in class") {
    val error = """|<input>:1: error: `]` expected but `identifier` found
                   |class A[`-`T](t: T)
                   |           ^""".stripMargin
    runTestError[Stat]("class A[`-`T](t: T)", error)
  }

  test("contravariant in type") {
    assertTree(templStat("type A[-T] = B[T]")) {
      Defn.Type(
        Nil,
        pname("A"),
        pparam(List(Mod.Contravariant()), "T") :: Nil,
        Type.Apply(pname("B"), List(pname("T"))),
        noBounds
      )
    }
  }

  test("contravariant in def")(interceptParseError("def foo[-T](t: T): Int"))

  test("val param in case class") {
    assertTree(templStat("case class A(val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(tparam(List(Mod.ValParam()), "a", "Int")),
        tplNoBody()
      )
    }
  }

  test("val param in class") {
    assertTree(templStat("class A(val a: Int)")) {
      Defn.Class(Nil, pname("A"), Nil, ctorp(tparam(List(Mod.ValParam()), "a", "Int")), tplNoBody())
    }
  }

  test("implicit val param in case class") {
    assertTree(templStat("case class A(implicit val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(Mod.Implicit(), tparam(List(Mod.Implicit(), Mod.ValParam()), "a", "Int")),
        tplNoBody()
      )
    }
  }

  test("implicit val param in class") {
    assertTree(templStat("class A(implicit val a: Int)")) {
      Defn.Class(
        Nil,
        pname("A"),
        Nil,
        ctorp(Mod.Implicit(), tparam(List(Mod.Implicit(), Mod.ValParam()), "a", "Int")),
        tplNoBody()
      )
    }
  }

  test("no val param in def") {
    // No ValParam detected inside parameter list
    assertTree(templStat("def foo(a: Int): Int = a")) {
      Defn
        .Def(Nil, tname("foo"), Nil, List(tparam("a", "Int")) :: Nil, Some(pname("Int")), tname("a"))
    }
  }

  test("val param in def")(interceptParseError("def foo(val a: Int): Int"))

  test("var param in case class") {
    assertTree(templStat("case class A(var a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(tparam(List(Mod.VarParam()), "a", "Int")),
        tplNoBody()
      )
    }
  }

  test("var param in class") {
    assertTree(templStat("class A(var a: Int)")) {
      Defn.Class(Nil, pname("A"), Nil, ctorp(tparam(List(Mod.VarParam()), "a", "Int")), tplNoBody())
    }
  }

  test("implicit var param in case class") {
    assertTree(templStat("case class A(implicit var a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        ctorp(Mod.Implicit(), tparam(List(Mod.Implicit(), Mod.VarParam()), "a", "Int")),
        tplNoBody()
      )
    }
  }

  test("implicit var param in class") {
    assertTree(templStat("class A(implicit var a: Int)")) {
      Defn.Class(
        Nil,
        pname("A"),
        Nil,
        ctorp(Mod.Implicit(), tparam(List(Mod.Implicit(), Mod.VarParam()), "a", "Int")),
        tplNoBody()
      )
    }
  }

  test("var param in def")(interceptParseError("def foo(var a: Int): Int"))

  test("macro") {
    matchSubStructure[Stat](
      "def foo(a: Int): Int = macro myMacroImpl(a)",
      { case Defn.Macro(_, _, _, _, _, _) => () }
    )
  }

  test("final and abstract") {
    // Only check these because abstract cannot only be used for classes
    matchSubStructure[Stat](
      "final abstract class A",
      { case Defn.Class(List(Mod.Final(), Mod.Abstract()), _, _, _, _) => () }
    )
    matchSubStructure[Stat](
      "final abstract case class A(a: Int)",
      { case Defn.Class(List(Mod.Final(), Mod.Abstract(), Mod.Case()), _, _, _, _) => () }
    )
  }

  test("not really invalid private and protected") {
    // NOTE: Surprisingly, the code below is valid Scala.
    matchSubStructure[Stat](
      "private protected[foo] def foo = ???",
      {
        case Defn.Def(
              List(Mod.Private(anon), Mod.Protected(Name.Indeterminate("foo"))),
              _,
              _,
              _,
              _,
              _
            ) => ()
      }
    )

    matchSubStructure[Stat](
      "private[foo] protected def foo = ???",
      {
        case Defn.Def(
              List(Mod.Private(Name.Indeterminate("foo")), Mod.Protected(anon)),
              _,
              _,
              _,
              _,
              _
            ) => ()
      }
    )

    matchSubStructure[Stat](
      "protected private[foo] def foo = ???",
      {
        case Defn.Def(
              List(Mod.Protected(anon), Mod.Private(Name.Indeterminate("foo"))),
              _,
              _,
              _,
              _,
              _
            ) => ()
      }
    )

    matchSubStructure[Stat](
      "protected[foo] private def foo = ???",
      {
        case Defn.Def(
              List(Mod.Protected(Name.Indeterminate("foo")), Mod.Private(anon)),
              _,
              _,
              _,
              _,
              _
            ) => ()
      }
    )

  }

  test("Annotation after modifier")(interceptParseError("implicit @foo def foo(a: Int): Int"))

  test("missing val after parameter modifier") {
    val actual = interceptParseError("class A(implicit b: B, implicit c: C)")
    val expected = s"""|error: `val` expected but `identifier` found
                       |class A(implicit b: B, implicit c: C)
                       |                                ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("repeated parameter modifier on first parameter") {
    val actual = interceptParseError("class A(implicit implicit val b: B)")
    val expected = s"""|error: repeated modifier
                       |class A(implicit implicit val b: B)
                       |                 ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("by-name parameter: class with val") {
    val actual = interceptParseError("class A(val b: => B)")
    val expected = s"""|error: `val' parameters may not be call-by-name
                       |class A(val b: => B)
                       |            ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("by-name parameter: class with private[this] val") {
    assertNoDiff(
      templStat("class A(private[this] val b: => B)").syntax,
      "class A(private[this] val b: => B)"
    )
  }

  test("by-name parameter: case class with val") {
    val actual = interceptParseError("case class A(val b: => B)")
    val expected = s"""|error: `val' parameters may not be call-by-name
                       |case class A(val b: => B)
                       |                 ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("by-name parameter: class with implicit val") {
    val actual = interceptParseError("class A(implicit val b: => B)")
    val expected = s"""|error: `val' parameters may not be call-by-name
                       |class A(implicit val b: => B)
                       |                     ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("#3122 missing val after package-private modifier") {
    val code = "case class Foo(private[example] field: String)"
    val expected = Defn.Class(
      List(Mod.Case()),
      pname("Foo"),
      Nil,
      ctorp(tparam(List(Mod.Private(Name("example"))), "field", "String")),
      tplNoBody()
    )
    assertTree(templStat(code))(expected)
  }

}
