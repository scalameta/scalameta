package scala.meta.tests
package parsers

import org.scalatest.exceptions.TestFailedException

import scala.meta._

class ModSuite extends ParseSuite {
  test("implicit") {
    val Defn.Object(Seq(Mod.Implicit()), _, _) = templStat("implicit object A")
    val Defn.Class(Seq(Mod.Implicit()), _, _, _, _) = templStat("implicit class A")
    val Defn.Object(Seq(Mod.Implicit(), Mod.Case()), _, _) = templStat("implicit case object A")
    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Implicit(), Mod.ValParam()), _, _, _)))
    ), _) = templStat("case class A(implicit val a: Int)")
    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Implicit(), Mod.VarParam()), _, _, _)))
    ), _) = templStat("case class A(implicit var a: Int)")

    val Defn.Def(_, _, _, Seq(Seq(Term.Param(Seq(Mod.Implicit()), _, _, _))), _, _) =
      templStat("def foo(implicit a: Int): Int = a")

    val Defn.Def(Seq(Mod.Implicit()), _, _, _, _, _) = templStat("implicit def foo(a: Int): Int = a")

    val Defn.Val(Seq(Mod.Implicit()), _, _, _) = templStat("implicit val a: Int = 1")
    val Decl.Val(Seq(Mod.Implicit()), _, _) = templStat("implicit val a: Int")

    val Defn.Var(Seq(Mod.Implicit()), _, _, _) = templStat("implicit var a: Int = 1")
    val Decl.Var(Seq(Mod.Implicit()), _, _) = templStat("implicit var a: Int")

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
    val Defn.Object(Seq(Mod.Final()), _, _) = templStat("final object A")
    val Defn.Class(Seq(Mod.Final()), _, _, _, _) = templStat("final class A")
    val Defn.Class(Seq(Mod.Final(), Mod.Case()), _, _, _, _) = templStat("final case class A(a: Int)")
    val Defn.Object(Seq(Mod.Final(), Mod.Case()), _, _) = templStat("final case object A")
    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Final(), Mod.ValParam()), _, _, _)))
    ), _) = templStat("case class A(final val a: Int)")

    val Defn.Def(Seq(Mod.Final()), _, _, _, _, _) = templStat("final def foo(a: Int): Int = a")
    val Defn.Val(Seq(Mod.Final()), _, _, _) = templStat("final val a: Int = 1")
    val Decl.Val(Seq(Mod.Final()), _, _) = templStat("final val a: Int")

    val Defn.Var(Seq(Mod.Final()), _, _, _) = templStat("final var a: Int = 1")
    val Decl.Var(Seq(Mod.Final()), _, _) = templStat("final var a: Int")
    val Defn.Type(Seq(Mod.Final()), _, _, _) = templStat("final type A = Int")

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
    val Defn.Trait(Seq(Mod.Sealed()), _, _, _, _) = templStat("sealed trait A")
    val Defn.Class(Seq(Mod.Sealed()), _, _, _, _) = templStat("sealed class A")
    val Defn.Class(Seq(Mod.Sealed(), Mod.Abstract()), _, _, _, _) = templStat("sealed abstract class A")
    val Defn.Class(Seq(Mod.Sealed(), Mod.Case()), _, _, _, _) = templStat("sealed case class A(a: Int)")

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
    val Defn.Object(Seq(Mod.Override()), _, _) = templStat("override object A")
    val Defn.Object(Seq(Mod.Override(), Mod.Case()), _, _) = templStat("override case object A")

    val Defn.Def(Seq(Mod.Override()), _, _, _, _, _) = templStat("override def foo(a: Int): Int = a")
    val Defn.Val(Seq(Mod.Override()), _, _, _) = templStat("override val a: Int = 1")
    val Defn.Var(Seq(Mod.Override()), _, _, _) = templStat("override var a: Int = 1")
    val Defn.Type(Seq(Mod.Override()), _, _, _) = templStat("override type A = Int")

    val Decl.Def(Seq(Mod.Override()), _, _, _, _) = templStat("override def foo(a: Int): Int")
    val Decl.Val(Seq(Mod.Override()), _, _) = templStat("override val a: Int")
    val Decl.Var(Seq(Mod.Override()), _, _) = templStat("override var a: Int")
    val Decl.Type(Seq(Mod.Override()), _, _, _) = templStat("override type A")

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
      "override class A",
      "override case class A(a: Int)",
      "override trait A"
    )
  }

  test("case") {
    val Defn.Object(Seq(Mod.Case()), _, _) = templStat("case object A")
    val Defn.Class(Seq(Mod.Case()), _, _, _, _) = templStat("case class A(a: Int)")

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
    val Defn.Trait(Seq(Mod.Abstract()), _, _, _, _) = templStat("abstract trait A")
    val Defn.Class(Seq(Mod.Abstract()), _, _, _, _) = templStat("abstract class A")
    val Defn.Class(Seq(Mod.Abstract(), Mod.Case()), _, _, _, _) = templStat("abstract case class A(a: Int)")

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
    val Defn.Val(Seq(Mod.Lazy()), _, _, _) = templStat("lazy val a: Int = 1")

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
    val Defn.Object(Seq(Mod.Abstract(), Mod.Override()), _, _) = templStat("abstract override object A")
    val Defn.Object(Seq(Mod.Abstract(), Mod.Override(), Mod.Case()), _, _) = templStat("abstract override case object A")

    val Defn.Def(Seq(Mod.Abstract(), Mod.Override()), _, _, _, _, _) = templStat("abstract override def foo(a: Int): Int = a")
    val Defn.Val(Seq(Mod.Abstract(), Mod.Override()), _, _, _) = templStat("abstract override val a: Int = 1")
    val Defn.Var(Seq(Mod.Abstract(), Mod.Override()), _, _, _) = templStat("abstract override var a: Int = 1")
    val Defn.Type(Seq(Mod.Abstract(), Mod.Override()), _, _, _) = templStat("abstract override type A = Int")

    val Decl.Def(Seq(Mod.Abstract(), Mod.Override()), _, _, _, _) = templStat("abstract override def foo(a: Int): Int")
    val Decl.Val(Seq(Mod.Abstract(), Mod.Override()), _, _) = templStat("abstract override val a: Int")
    val Decl.Var(Seq(Mod.Abstract(), Mod.Override()), _, _) = templStat("abstract override var a: Int")
    val Decl.Type(Seq(Mod.Abstract(), Mod.Override()), _, _, _) = templStat("abstract override type A")

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
      "def foo(abstract override val a: Int): Int = a",
      "abstract override class A",
      "abstract override case class A(a: Int)"
    )
  }

  test("covariant") {
    val Defn.Class(_, _,
      Seq(Type.Param(Seq(Mod.Covariant()), _, _, _, _, _)),
    _, _) = templStat("case class A[+T](t: T)")

    val Defn.Class(_, _,
      Seq(Type.Param(Seq(Mod.Covariant()), _, _, _, _, _)),
    _, _) = templStat("class A[+T](t: T)")

    val Defn.Type(_, _,
      Seq(Type.Param(Seq(Mod.Covariant()), _, _, _, _, _)),
    _) = templStat("type A[+T] = B[T]")

    interceptParseErrors(
      "def foo[+T](t: T): Int"
    )
  }

  test("contravariant") {
    val Defn.Class(_, _,
      Seq(Type.Param(Seq(Mod.Contravariant()), _, _, _, _, _)),
    _, _) = templStat("case class A[-T](t: T)")

    val Defn.Class(_, _,
      Seq(Type.Param(Seq(Mod.Contravariant()), _, _, _, _, _)),
    _, _) = templStat("class A[-T](t: T)")

    val Defn.Type(_, _,
      Seq(Type.Param(Seq(Mod.Contravariant()), _, _, _, _, _)),
    _) = templStat("type A[-T] = B[T]")

    interceptParseErrors(
      "def foo[-T](t: T): Int"
    )
  }

  test("val param") {
    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.ValParam()), _, _, _)))
    ), _) = templStat("case class A(val a: Int)")

    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.ValParam()), _, _, _)))
    ), _) = templStat("class A(val a: Int)")

    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Implicit(), Mod.ValParam()), _, _, _)))
    ), _) = templStat("case class A(implicit val a: Int)")

    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Implicit(), Mod.ValParam()), _, _, _)))
    ), _) = templStat("class A(implicit val a: Int)")

    // No ValParam detected inside parameter list
    val Defn.Def(_, _, _, Seq(Seq(Term.Param(Seq(), _, _, _))), _, _) =
      templStat("def foo(a: Int): Int = a")

    interceptParseErrors(
      "def foo(val a: Int): Int"
    )
  }

  test("var param") {
    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.VarParam()), _, _, _)))
    ), _) = templStat("case class A(var a: Int)")

    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.VarParam()), _, _, _)))
    ), _) = templStat("class A(var a: Int)")

    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Implicit(), Mod.VarParam()), _, _, _)))
    ), _) = templStat("case class A(implicit var a: Int)")

    val Defn.Class(_, _, _, Ctor.Primary(_, _,
      Seq(Seq(Term.Param(Seq(Mod.Implicit(), Mod.VarParam()), _, _, _)))
    ), _) = templStat("class A(implicit var a: Int)")

    interceptParseErrors(
      "def foo(var a: Int): Int"
    )
  }

  test("macro") {
    val Defn.Macro(_, _, _, _, _, _) = templStat("def foo(a: Int): Int = macro myMacroImpl(a)")
  }

  test("final and abstract") {
    // Only check these because abstract cannot only be used for classes
    val Defn.Class(Seq(Mod.Final(), Mod.Abstract()), _, _, _, _) = templStat("final abstract class A")
    val Defn.Class(Seq(Mod.Final(), Mod.Abstract(), Mod.Case()), _, _, _, _) = templStat("final abstract case class A(a: Int)")

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
      "private protected class A",
      "protected private class A",
      "private[something] protected class A",
      "protected private[something] class A",
      "protected[something] private class A",
      "private protected[something] class A",
      "protected protected class A",
      "private private class A"
    )
  }
}
