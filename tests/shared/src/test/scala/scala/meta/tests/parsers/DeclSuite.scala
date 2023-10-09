package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211

class DeclSuite extends ParseSuite {
  test("val x: Int") {
    assertTree(templStat("val x: Int"))(
      Decl.Val(Nil, List(Pat.Var(Term.Name("x"))), Type.Name("Int"))
    )
  }

  test("var x: Int") {
    assertTree(templStat("var x: Int"))(
      Decl.Var(Nil, List(Pat.Var(Term.Name("x"))), Type.Name("Int"))
    )
  }

  test("val x, y: Int") {
    assertTree(templStat("val x, y: Int"))(
      Decl.Val(Nil, List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))), Type.Name("Int"))
    )
    assertTree(templStat("var x, y: Int"))(
      Decl.Var(Nil, List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))), Type.Name("Int"))
    )
  }

  test("var x, y: Int") {
    assertTree(templStat("var x, y: Int"))(
      Decl.Var(Nil, List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))), Type.Name("Int"))
    )
  }

  test("type T") {
    assertTree(templStat("type T")) {
      Decl.Type(Nil, Type.Name("T"), Type.ParamClause(Nil), Type.Bounds(None, None))
    }
  }

  test("type T <: hi") {
    assertTree(templStat("type T <: hi")) {
      Decl.Type(
        Nil,
        Type.Name("T"),
        Type.ParamClause(Nil),
        Type.Bounds(None, Some(Type.Name("hi")))
      )
    }
  }

  test("type T >: lo") {
    assertTree(templStat("type T >: lo")) {
      Decl.Type(
        Nil,
        Type.Name("T"),
        Type.ParamClause(Nil),
        Type.Bounds(Some(Type.Name("lo")), None)
      )
    }
  }

  test("type T >: lo <: hi") {
    assertTree(templStat("type T >: lo <: hi")) {
      Decl.Type(
        Nil,
        Type.Name("T"),
        Type.ParamClause(Nil),
        Type.Bounds(Some(Type.Name("lo")), Some(Type.Name("hi")))
      )
    }
  }

  test("type F[T]") {
    assertTree(templStat("type F[T]")) {
      Decl.Type(
        Nil,
        Type.Name("F"),
        Type.ParamClause(
          Type.Param(
            Nil,
            Type.Name("T"),
            Type.ParamClause(Nil),
            Type.Bounds(None, None),
            Nil,
            Nil
          ) :: Nil
        ),
        Type.Bounds(None, None)
      )
    }
  }

  test("type F[_]") {
    assertTree(templStat("type F[_]")) {
      Decl.Type(
        Nil,
        Type.Name("F"),
        Type.ParamClause(
          Type.Param(
            Nil,
            Name.Placeholder(),
            Type.ParamClause(Nil),
            Type.Bounds(None, None),
            Nil,
            Nil
          ) :: Nil
        ),
        Type.Bounds(None, None)
      )
    }
  }

  test("type F[A <: B]") {
    assertTree(templStat("type F[T <: B]")) {
      Decl.Type(
        Nil,
        Type.Name("F"),
        Type.ParamClause(
          Type.Param(
            Nil,
            Type.Name("T"),
            Type.ParamClause(Nil),
            Type.Bounds(None, Some(Type.Name("B"))),
            Nil,
            Nil
          ) :: Nil
        ),
        Type.Bounds(None, None)
      )
    }
  }

  test("type F[+T]") {
    assertTree(templStat("type F[+T]")) {
      Decl.Type(
        Nil,
        Type.Name("F"),
        Type.ParamClause(
          Type.Param(
            Mod.Covariant() :: Nil,
            Type.Name("T"),
            Type.ParamClause(Nil),
            Type.Bounds(None, None),
            Nil,
            Nil
          ) :: Nil
        ),
        Type.Bounds(None, None)
      )
    }
    assertTree(templStat("type F[-T]")) {
      Decl.Type(
        Nil,
        Type.Name("F"),
        Type.ParamClause(
          Type.Param(
            Mod.Contravariant() :: Nil,
            Type.Name("T"),
            Type.ParamClause(Nil),
            Type.Bounds(None, None),
            Nil,
            Nil
          ) :: Nil
        ),
        Type.Bounds(None, None)
      )
    }
  }

  test("def f") {
    assertTree(templStat("def f")) {
      Decl.Def(Nil, Term.Name("f"), Type.ParamClause(Nil), Nil, Type.Name("Unit"))
    }
  }

  test("def f(x: Int)") {
    assertTree(templStat("def f(x: Int)")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Type.ParamClause(Nil),
        (Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil,
        Type.Name("Unit")
      )
    }
  }

  test("def f(x: Int*)") {
    assertTree(templStat("def f(x: Int*)")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Type.ParamClause(Nil),
        (Term
          .Param(Nil, Term.Name("x"), Some(Type.Repeated(Type.Name("Int"))), None) :: Nil) :: Nil,
        Type.Name("Unit")
      )
    }
  }

  test("def f(x: => Int)") {
    assertTree(templStat("def f(x: => Int)")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Type.ParamClause(Nil),
        (Term.Param(Nil, Term.Name("x"), Some(Type.ByName(Type.Name("Int"))), None) :: Nil) :: Nil,
        Type.Name("Unit")
      )
    }
  }

  test("def f(implicit x: Int)") {
    assertTree(templStat("def f(implicit x: Int)")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        (Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil)
          :: Nil,
        Type.Name("Unit")
      )
    }
  }

  test("def f: X") {
    assertTree(templStat("def f: X")) {
      Decl.Def(Nil, Term.Name("f"), Type.ParamClause(Nil), Nil, Type.Name("X"))
    }
  }

  test("def f[T]: T") {
    assertTree(templStat("def f[T]: T")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Type.ParamClause(
          Type.Param(
            Nil,
            Type.Name("T"),
            Type.ParamClause(Nil),
            Type.Bounds(None, None),
            Nil,
            Nil
          ) :: Nil
        ),
        Nil,
        Type.Name("T")
      )
    }
  }
}
