package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDeclSuite extends BaseDottySuite {

  import dialects.Scala3Future

  override protected val dialect: Dialect = Scala3Future

  test("def f: Unit") {
    checkTree(templStat("def f: Unit"))(Decl.Def(Nil, tname("f"), Nil, pname("Unit")))
  }

  test("def f(x: Int): Unit") {
    checkTree(templStat("def f(x: Int): Unit")) {
      Decl.Def(Nil, tname("f"), Nil, List(tparam("x", "Int") :: Nil), pname("Unit"))
    }
  }

  test("def f(x: Int*): Unit") {
    checkTree(templStat("def f(x: Int*): Unit")) {
      Decl.Def(
        Nil,
        tname("f"),
        Nil,
        List(tparam("x", Type.Repeated(pname("Int"))) :: Nil),
        pname("Unit")
      )
    }
  }

  test("def f(x: => Int): Unit") {
    checkTree(templStat("def f(x: => Int): Unit")) {
      Decl
        .Def(Nil, tname("f"), Nil, List(tparam("x", Type.ByName(pname("Int"))) :: Nil), pname("Unit"))
    }
  }

  test("def f(implicit x: Int): Unit") {
    checkTree(templStat("def f(implicit x: Int): Unit")) {
      Decl.Def(
        Nil,
        tname("f"),
        Nil,
        List(tparam(Mod.Implicit() :: Nil, "x", "Int") :: Nil),
        pname("Unit")
      )
    }
  }

  test("def f: X")(checkTree(templStat("def f: X"))(Decl.Def(Nil, tname("f"), Nil, pname("X"))))

  test("def f[T]: T") {
    checkTree(templStat("def f[T]: T")) {
      Decl.Def(Nil, tname("f"), pparam("T") :: Nil, Nil, pname("T"))
    }
  }

  test("def f[A][B]: A") {
    runTestError[Stat](
      "def f[A][B]: A",
      """|error: `=` expected but `[` found
         |def f[A][B]: A
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B]: B") {
      Decl.Def(
        Nil,
        tname("f"),
        pcg(List(pparam("A")), List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A"))))) ::
          pcg(List(pparam("B"))) :: Nil,
        pname("B")
      )
    }
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B",
      """|error: `=` expected but `[` found
         |def f[A](implicit a: A)[B](implicit b: B): B
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
      Decl.Def(
        Nil,
        tname("f"),
        pcg(List(pparam("A")), List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A"))))) ::
          pcg(List(pparam("B")), List(tparam("b", "B"), tparam("bs", Type.Repeated(pname("B"))))) ::
          pcg(List(pparam("C")), List(tparam(List(Mod.Implicit()), "c", "C"))) :: Nil,
        pname("B")
      )
    }
  }
}
