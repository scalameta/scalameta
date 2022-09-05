package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211

class DeclSuite extends ParseSuite {
  test("val x: Int") {
    val Decl.Val(Nil, List(Pat.Var(Term.Name("x"))), Type.Name("Int")) = templStat("val x: Int")
  }

  test("var x: Int") {
    val Decl.Var(Nil, List(Pat.Var(Term.Name("x"))), Type.Name("Int")) = templStat("var x: Int")
  }

  test("val x, y: Int") {
    val Decl.Val(Nil, List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))), Type.Name("Int")) =
      templStat("val x, y: Int")
    val Decl.Var(Nil, List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))), Type.Name("Int")) =
      templStat("var x, y: Int")
  }

  test("var x, y: Int") {
    val Decl.Var(Nil, List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))), Type.Name("Int")) =
      templStat("var x, y: Int")
  }

  test("type T") {
    val t @ Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(None, None)) = templStat("type T")
  }

  test("type T <: hi") {
    val t @ Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(None, Some(Type.Name("hi")))) =
      templStat("type T <: hi")
  }

  test("type T >: lo") {
    val t @ Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(Some(Type.Name("lo")), None)) =
      templStat("type T >: lo")
  }

  test("type T >: lo <: hi") {
    val t @ Decl.Type(
      Nil,
      Type.Name("T"),
      Nil,
      Type.Bounds(Some(Type.Name("lo")), Some(Type.Name("hi")))
    ) = templStat("type T >: lo <: hi")
  }

  test("type F[T]") {
    val Decl.Type(
      Nil,
      Type.Name("F"),
      Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
      Type.Bounds(None, None)
    ) = templStat("type F[T]")
  }

  test("type F[_]") {
    val Decl.Type(
      Nil,
      Type.Name("F"),
      Type.Param(Nil, Name.Anonymous(), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
      Type.Bounds(None, None)
    ) = templStat("type F[_]")
  }

  test("type F[A <: B]") {
    val Decl.Type(
      Nil,
      Type.Name("F"),
      Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, Some(Type.Name("B"))), Nil, Nil)
        :: Nil,
      Type.Bounds(None, None)
    ) = templStat("type F[T <: B]")
  }

  test("type F[+T]") {
    val Decl.Type(
      Nil,
      Type.Name("F"),
      Type.Param(Mod.Covariant() :: Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
        :: Nil,
      Type.Bounds(None, None)
    ) = templStat("type F[+T]")
    val Decl.Type(
      Nil,
      Type.Name("F"),
      Type.Param(Mod.Contravariant() :: Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
        :: Nil,
      Type.Bounds(None, None)
    ) = templStat("type F[-T]")
  }

  test("def f") {
    val Decl.Def(Nil, Term.Name("f"), Nil, Type.Name("Unit")) = templStat("def f")
  }

  test("def f(x: Int)") {
    val Decl.Def(
      Nil,
      Term.Name("f"),
      Clause.TermClause(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil,
      Type.Name("Unit")
    ) =
      templStat("def f(x: Int)")
  }

  test("def f(x: Int*)") {
    val Decl.Def(
      Nil,
      Term.Name("f"),
      Clause.TermClause(Term.Param(Nil, Term.Name("x"), Some(Type.Repeated(Type.Name("Int"))), None) :: Nil) :: Nil,
      Type.Name("Unit")
    ) =
      templStat("def f(x: Int*)")
  }

  test("def f(x: => Int)") {
    val Decl.Def(
      Nil,
      Term.Name("f"),
      Clause.TermClause(Term.Param(Nil, Term.Name("x"), Some(Type.ByName(Type.Name("Int"))), None) :: Nil) :: Nil,
      Type.Name("Unit")
    ) =
      templStat("def f(x: => Int)")
  }

  test("def f(implicit x: Int)") {
    val Decl.Def(
      Nil,
      Term.Name("f"),
      Clause.TermClause(Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil)
        :: Nil,
      Type.Name("Unit")
    ) =
      templStat("def f(implicit x: Int)")
  }

  test("def f: X") {
    val Decl.Def(Nil, Term.Name("f"), Nil, Type.Name("X")) =
      templStat("def f: X")
  }

  test("def f[T]: T") {
    val Decl.Def(
      Nil,
      Term.Name("f"),
      Clause.TypeClause(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil) :: Nil,
      Type.Name("T")
    ) =
      templStat("def f[T]: T")
  }
}
