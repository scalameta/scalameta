package scala.meta.tests
package parsers

import scala.meta._

class ModSuite extends ParseSuite {
  test("implicit") {
    assertTree(templStat("implicit object A"))(
      Defn.Object(List(Mod.Implicit()), tname("A"), EmptyTemplate())
    )
    assertTree(templStat("implicit class A"))(
      Defn.Class(
        List(Mod.Implicit()),
        pname("A"),
        Nil,
        EmptyCtor(),
        EmptyTemplate()
      )
    )
    assertTree(templStat("implicit case object A"))(
      Defn.Object(
        List(Mod.Implicit(), Mod.Case()),
        tname("A"),
        EmptyTemplate()
      )
    )

    assertTree(templStat("case class A(implicit val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Implicit(), Mod.ValParam()), "a", "Int") :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        EmptyTemplate()
      )
    }
    assertTree(templStat("case class A(implicit var a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Implicit(), Mod.VarParam()), "a", "Int") :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        EmptyTemplate()
      )
    }

    assertTree(templStat("def foo(implicit a: Int): Int = a")) {
      Defn.Def(
        Nil,
        tname("foo"),
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(
            List(tparam(List(Mod.Implicit()), "a", "Int")),
            Some(Mod.Implicit())
          ) :: Nil
        ) :: Nil,
        Some(pname("Int")),
        tname("a")
      )
    }

    assertTree(templStat("implicit def foo(a: Int): Int = a")) {
      Defn.Def(
        List(Mod.Implicit()),
        tname("foo"),
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(tparam("a", "Int")) :: Nil
        ) :: Nil,
        Some(pname("Int")),
        tname("a")
      )
    }

    assertTree(templStat("implicit val a: Int = 1")) {
      Defn.Val(
        List(Mod.Implicit()),
        List(Pat.Var(tname("a"))),
        Some(pname("Int")),
        int(1)
      )
    }

    assertTree(templStat("implicit val a: Int")) {
      Decl.Val(List(Mod.Implicit()), List(Pat.Var(tname("a"))), pname("Int"))
    }

    assertTree(templStat("implicit var a: Int = 1")) {
      Defn.Var(
        List(Mod.Implicit()),
        List(Pat.Var(tname("a"))),
        Some(pname("Int")),
        Some(int(1))
      )

    }

    assertTree(templStat("implicit var a: Int")) {
      Decl.Var(List(Mod.Implicit()), List(Pat.Var(tname("a"))), pname("Int"))
    }

    interceptParseErrors(
      "implicit implicit var a: Int",
      "implicit implicit val a: Int",
      "implicit implicit var a: Int = 1",
      "implicit implicit val a: Int = 1",
      "implicit implicit class A",
      "implicit implicit object A",
      "implicit implicit trait A",
      "implicit implicit case class A(a: Int)",
      "implicit implicit type A",
      "implicit implicit type A = Int",
      "implicit trait A",
      "implicit type A",
      "implicit type A = Int",
      "implicit case class A(a: Int)"
    )
  }

  test("final") {
    matchSubStructure[Stat](
      "final object A",
      { case Defn.Object(List(Mod.Final()), _, _) => () }
    )
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
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Final(), Mod.ValParam()), "a", "Int") :: Nil
          ) :: Nil
        ),
        EmptyTemplate()
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

    matchSubStructure[Stat](
      "final val a: Int",
      { case Decl.Val(List(Mod.Final()), _, _) => () }
    )

    matchSubStructure[Stat](
      "final var a: Int = 1",
      { case Defn.Var(List(Mod.Final()), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "final var a: Int",
      { case Decl.Var(List(Mod.Final()), _, _) => () }
    )

    matchSubStructure[Stat](
      "final type A = Int",
      { case Defn.Type(List(Mod.Final()), _, _, _) => () }
    )

    interceptParseErrors(
      "final final var a: Int",
      "final final val a: Int",
      "final final var a: Int = 1",
      "final final val a: Int = 1",
      "final final class A",
      "final final object A",
      "final final trait A",
      "final final case class A(a: Int)",
      "final final type A",
      "final trait A",
      "def foo(final val a: Int): Int = a"
    )
  }

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

    interceptParseErrors(
      "sealed sealed var a: Int",
      "sealed sealed val a: Int",
      "sealed sealed var a: Int = 1",
      "sealed sealed val a: Int = 1",
      "sealed sealed class A",
      "sealed sealed object A",
      "sealed sealed trait A",
      "sealed sealed case class A(a: Int)",
      "sealed sealed type A",
      "sealed object A",
      "sealed case object A",
      "sealed def foo(a: Int): Int = a",
      "sealed val a: Int = 1",
      "sealed val a: Int",
      "sealed var a: Int = 1",
      "sealed var a: Int",
      "sealed type A",
      "sealed type A = Int",
      "def foo(sealed val a: Int): Int = a",
      "class A(sealed val a: Int)"
    )
  }

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

    interceptParseErrors(
      "override override var a: Int",
      "override override val a: Int",
      "override override var a: Int = 1",
      "override override val a: Int = 1",
      "override override class A",
      "override override object A",
      "override override trait A",
      "override override case class A(a: Int)",
      "override override type A",
      "def foo(override val a: Int): Int = a",
      "override trait A"
    )
  }

  test("case") {
    matchSubStructure[Stat](
      "case object A",
      { case Defn.Object(List(Mod.Case()), _, _) => () }
    )

    matchSubStructure[Stat](
      "case class A(a: Int)",
      { case Defn.Class(List(Mod.Case()), _, _, _, _) => () }
    )
    interceptParseErrors(
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
  }

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

    interceptParseErrors(
      "abstract abstract var a: Int",
      "abstract abstract val a: Int",
      "abstract abstract var a: Int = 1",
      "abstract abstract val a: Int = 1",
      "abstract abstract class A",
      "abstract abstract object A",
      "abstract abstract trait A",
      "abstract abstract case class A(a: Int)",
      "abstract abstract type A",
      "abstract val a: Int",
      "abstract var a: Int",
      "abstract val a: Int = 1",
      "abstract var a: Int = 1",
      "abstract def foo(a: Int): Int",
      "abstract type A",
      "abstract type A = Int",
      "class A(abstract val a: Int)",
      "def foo(abstract val a: Int): Int = a",
      "abstract def foo(val a: Int): Int = a",
      "abstract case object A",
      "abstract object A"
    )
  }

  test("lazy") {
    matchSubStructure[Stat](
      "lazy val a: Int = 1",
      { case Defn.Val(List(Mod.Lazy()), _, _, _) => () }
    )

    interceptParseErrors(
      "lazy lazy var a: Int",
      "lazy lazy val a: Int",
      "lazy lazy var a: Int = 1",
      "lazy lazy val a: Int = 1",
      "lazy lazy class A",
      "lazy lazy object A",
      "lazy lazy trait A",
      "lazy lazy case class A(a: Int)",
      "lazy lazy type A",
      "lazy val a: Int",
      "lazy var a: Int",
      "lazy var a: Int = 1",
      "lazy def foo(a: Int): Int",
      "lazy type A",
      "lazy type A = Int",
      "def foo(lazy val a: Int): Int = a",
      "class A(lazy val a: Int)",
      "lazy def foo(val a: Int): Int = a",
      "lazy case object A",
      "lazy case class A(a: Int)",
      "lazy class A",
      "lazy object A"
    )
  }

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

    interceptParseErrors(
      "abstract override abstract override var a: Int",
      "abstract override abstract override val a: Int",
      "abstract override abstract override var a: Int = 1",
      "abstract override abstract override val a: Int = 1",
      "abstract override abstract override class A",
      "abstract override abstract override object A",
      "abstract override abstract override trait A",
      "abstract override abstract override case class A(a: Int)",
      "abstract override abstract override type A",
      "def foo(abstract override val a: Int): Int = a"
    )
  }

  test("covariant in case class") {
    assertTree(templStat("case class A[+T](t: T)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        pparam(List(Mod.Covariant()), "T") :: Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(tparam("t", "T")) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("covariant in class") {
    assertTree(templStat("class A[+T](t: T)")) {
      Defn.Class(
        Nil,
        pname("A"),
        pparam(List(Mod.Covariant()), "T") :: Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(tparam("t", "T")) :: Nil
        ),
        EmptyTemplate()
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

  test("covariant in def") {
    interceptParseError(
      "def foo[+T](t: T): Int"
    )
  }

  test("contravariant in case class") {
    assertTree(templStat("case class A[-T](t: T)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        pparam(List(Mod.Contravariant()), "T") :: Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(tparam("t", "T")) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("contravariant in class") {
    assertTree(templStat("class A[-T](t: T)")) {
      Defn.Class(
        Nil,
        pname("A"),
        pparam(List(Mod.Contravariant()), "T") :: Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(tparam("t", "T")) :: Nil
        ),
        EmptyTemplate()
      )
    }
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

  test("contravariant in def") {
    interceptParseError(
      "def foo[-T](t: T): Int"
    )
  }

  test("val param in case class") {
    assertTree(templStat("case class A(val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          List(tparam(List(Mod.ValParam()), "a", "Int")) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("val param in class") {
    assertTree(templStat("class A(val a: Int)")) {
      Defn.Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          List(tparam(List(Mod.ValParam()), "a", "Int")) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("implicit val param in case class") {
    assertTree(templStat("case class A(implicit val a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Implicit(), Mod.ValParam()), "a", "Int") :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("implicit val param in class") {
    assertTree(templStat("class A(implicit val a: Int)")) {
      Defn.Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Implicit(), Mod.ValParam()), "a", "Int") :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("no val param in def") {
    // No ValParam detected inside parameter list
    assertTree(templStat("def foo(a: Int): Int = a")) {
      Defn.Def(
        Nil,
        tname("foo"),
        Nil,
        List(tparam("a", "Int")) :: Nil,
        Some(pname("Int")),
        tname("a")
      )
    }
  }

  test("val param in def") {
    interceptParseError(
      "def foo(val a: Int): Int"
    )
  }

  test("var param in case class") {
    assertTree(templStat("case class A(var a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          List(tparam(List(Mod.VarParam()), "a", "Int")) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("var param in class") {
    assertTree(templStat("class A(var a: Int)")) {
      Defn.Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          List(tparam(List(Mod.VarParam()), "a", "Int")) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("implicit var param in case class") {
    assertTree(templStat("case class A(implicit var a: Int)")) {
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Implicit(), Mod.VarParam()), "a", "Int") :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("implicit var param in class") {
    assertTree(templStat("class A(implicit var a: Int)")) {
      Defn.Class(
        Nil,
        pname("A"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          anon,
          Term.ParamClause(
            tparam(List(Mod.Implicit(), Mod.VarParam()), "a", "Int") :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        EmptyTemplate()
      )
    }
  }

  test("var param in def") {
    interceptParseError(
      "def foo(var a: Int): Int"
    )
  }

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

    interceptParseErrors(
      "final abstract trait A",
      // Abstract should be inferred
      "final trait A"
    )
  }

  test("final and sealed") {
    // Only check these because sealed can only be used for classes
    interceptParseErrors(
      "final sealed class A(a: Int)",
      "final sealed case class A(a: Int)",
      "final sealed trait A"
    )
  }

  test("invalid private and protected") {
    interceptParseErrors(
      "private private class A",
      "private private[foo] class A",
      "private protected class A",
      // "private protected[foo] class A", /* see the test below */
      "private[foo] private class A",
      "private[foo] private[foo] class A",
      // "private[foo] protected class A", /* see the test below */
      "private[foo] protected[foo] class A",
      "protected private class A",
      // "protected private[foo] class A", /* see the test below */
      "protected protected class A",
      "protected protected[foo] class A",
      // "protected[foo] private class A", /* see the test below */
      "protected[foo] private[foo] class A",
      "protected[foo] protected class A",
      "protected[foo] protected[foo] class A"
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
            ) =>
          ()
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
            ) =>
          ()
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
            ) =>
          ()
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
            ) =>
          ()
      }
    )

  }

  test("Annotation after modifier") {
    interceptParseError("implicit @foo def foo(a: Int): Int")
  }

  test("missing val after parameter modifier") {
    val actual = interceptParseError("class A(implicit b: B, implicit c: C)")
    val expected =
      s"""|error: val expected but identifier found
          |class A(implicit b: B, implicit c: C)
          |                                ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("repeated parameter modifier on first parameter") {
    val actual = interceptParseError("class A(implicit implicit val b: B)")
    val expected =
      s"""|error: repeated modifier
          |class A(implicit implicit val b: B)
          |                 ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("repeated parameter modifier on second parameter") {
    val actual = interceptParseError("class A(implicit b: B, implicit implicit val c: C)")
    val expected =
      s"""|error: repeated modifier
          |class A(implicit b: B, implicit implicit val c: C)
          |                                ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("by-name parameter: class with val") {
    val actual = interceptParseError("class A(val b: => B)")
    val expected =
      s"""|error: `val' parameters may not be call-by-name
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
    val expected =
      s"""|error: `val' parameters may not be call-by-name
          |case class A(val b: => B)
          |                 ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("by-name parameter: class with implicit val") {
    val actual = interceptParseError("class A(implicit val b: => B)")
    val expected =
      s"""|error: `val' parameters may not be call-by-name
          |class A(implicit val b: => B)
          |                     ^""".stripMargin
    assert(actual.contains(expected), actual)
  }

  test("#3122 missing val after package-private modifier") {
    val code = "case class Foo(private[example] field: String)"
    val expected =
      Defn.Class(
        List(Mod.Case()),
        pname("Foo"),
        Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(tparam(List(Mod.Private(Name("example"))), "field", "String") :: Nil)
        ),
        EmptyTemplate()
      )
    assertTree(templStat(code))(expected)
  }

}
