package scala.meta.tests
package parsers

import scala.meta._

class DeclSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  test("val x: Int") {
    assertTree(templStat("val x: Int"))(Decl.Val(Nil, List(Pat.Var(tname("x"))), pname("Int")))
  }

  test("var x: Int") {
    assertTree(templStat("var x: Int"))(Decl.Var(Nil, List(Pat.Var(tname("x"))), pname("Int")))
  }

  test("val x, y: Int") {
    assertTree(templStat("val x, y: Int"))(
      Decl.Val(Nil, List(Pat.Var(tname("x")), Pat.Var(tname("y"))), pname("Int"))
    )
    assertTree(templStat("var x, y: Int"))(
      Decl.Var(Nil, List(Pat.Var(tname("x")), Pat.Var(tname("y"))), pname("Int"))
    )
  }

  test("var x, y: Int") {
    assertTree(templStat("var x, y: Int"))(
      Decl.Var(Nil, List(Pat.Var(tname("x")), Pat.Var(tname("y"))), pname("Int"))
    )
  }

  test("type T") {
    assertTree(templStat("type T")) {
      Decl.Type(Nil, pname("T"), Type.ParamClause(Nil), Type.Bounds(None, None))
    }
  }

  test("type T <: hi") {
    assertTree(templStat("type T <: hi")) {
      Decl.Type(Nil, pname("T"), Type.ParamClause(Nil), hiBound("hi"))
    }
  }

  test("type T >: lo") {
    assertTree(templStat("type T >: lo")) {
      Decl.Type(Nil, pname("T"), Type.ParamClause(Nil), loBound("lo"))
    }
  }

  test("type T >: lo <: hi") {
    assertTree(templStat("type T >: lo <: hi")) {
      Decl.Type(Nil, pname("T"), Type.ParamClause(Nil), bounds("lo", "hi"))
    }
  }

  test("type F[T]") {
    assertTree(templStat("type F[T]"))(Decl.Type(Nil, pname("F"), pparam("T") :: Nil, noBounds))
  }

  test("type F[_]") {
    assertTree(templStat("type F[_]"))(Decl.Type(Nil, pname("F"), pparam("_") :: Nil, noBounds))
  }

  test("type F[A <: B]") {
    assertTree(templStat("type F[T <: B]")) {
      Decl.Type(Nil, pname("F"), pparam("T", hiBound("B")) :: Nil, noBounds)
    }
  }

  test("type F[+T]") {
    assertTree(templStat("type F[+T]")) {
      Decl.Type(Nil, pname("F"), pparam(Mod.Covariant() :: Nil, "T") :: Nil, noBounds)
    }
    assertTree(templStat("type F[-T]")) {
      Decl.Type(Nil, pname("F"), pparam(Mod.Contravariant() :: Nil, "T") :: Nil, noBounds)
    }
  }

  test("def f") {
    assertTree(templStat("def f")) {
      Decl.Def(Nil, tname("f"), Type.ParamClause(Nil), Nil, pname("Unit"))
    }
  }

  test("def f(x: Int)") {
    assertTree(templStat("def f(x: Int)")) {
      Decl.Def(
        Nil,
        tname("f"),
        Type.ParamClause(Nil),
        (tparam("x", "Int") :: Nil) :: Nil,
        pname("Unit")
      )
    }
  }

  test("def f(x: Int*)") {
    assertTree(templStat("def f(x: Int*)")) {
      Decl.Def(
        Nil,
        tname("f"),
        Type.ParamClause(Nil),
        (tparam("x", Type.Repeated(pname("Int"))) :: Nil) :: Nil,
        pname("Unit")
      )
    }
  }

  test("def f(x: => Int)") {
    assertTree(templStat("def f(x: => Int)")) {
      Decl.Def(
        Nil,
        tname("f"),
        Type.ParamClause(Nil),
        (tparam("x", Type.ByName(pname("Int"))) :: Nil) :: Nil,
        pname("Unit")
      )
    }
  }

  test("def f(implicit x: Int)") {
    assertTree(templStat("def f(implicit x: Int)")) {
      Decl.Def(
        Nil,
        tname("f"),
        Nil,
        (tparam(Mod.Implicit() :: Nil, "x", "Int") :: Nil) :: Nil,
        pname("Unit")
      )
    }
  }

  test("def f: X") {
    assertTree(templStat("def f: X")) {
      Decl.Def(Nil, tname("f"), Type.ParamClause(Nil), Nil, pname("X"))
    }
  }

  test("def f[T]: T") {
    assertTree(templStat("def f[T]: T")) {
      Decl.Def(Nil, tname("f"), pparam("T") :: Nil, Nil, pname("T"))
    }
  }
}
