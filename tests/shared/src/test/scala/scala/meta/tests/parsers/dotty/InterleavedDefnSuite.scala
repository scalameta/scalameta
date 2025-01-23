package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDefnSuite extends BaseDottySuite {

  import dialects.Scala3Future

  override protected val dialect: Dialect = Scala3Future

  test("def x = 2")(checkTree(templStat("def x = 2"))(Defn.Def(Nil, tname("x"), Nil, None, int(2))))

  test("def x[A <: B] = 2") {
    checkTree(templStat("def x[A <: B] = 2")) {
      Defn.Def(Nil, tname("x"), pparam("A", hiBound("B")) :: Nil, Nil, None, int(2))
    }
  }

  test("def x[A: B] = 2") {
    checkTree(templStat("def x[A: B] = 2")) {
      Defn.Def(Nil, tname("x"), List(pparam("A", bounds(cb = List(pname("B"))))), Nil, None, int(2))
    }
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    checkTree(templStat("def f(a: Int)(implicit b: Int) = a + b")) {
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(tparam("a", "Int") :: Nil, tparam(Mod.Implicit() :: Nil, "b", "Int") :: Nil),
        None,
        tinfix(tname("a"), "+", tname("b"))
      )
    }
  }

  test("def f(x: Int) = macro impl") {
    checkTree(templStat("def f(x: Int) = macro impl")) {
      Defn.Macro(Nil, tname("f"), Nil, List(tparam(List(), "x", "Int") :: Nil), None, tname("impl"))
    }
  }

  test("def f(x: Int): Int = macro impl") {
    checkTree(templStat("def f(x: Int): Int = macro impl")) {
      Defn.Macro(
        Nil,
        tname("f"),
        Nil,
        List(tparam(List(), "x", "Int") :: Nil),
        Some(pname("Int")),
        tname("impl")
      )
    }
  }

  test("braces-in-functions") {
    val defn = templStat(
      """|def f = { (n: Int) =>
         |  {
         |    for {
         |      _ <- scala.util.Success(123)
         |    } yield 42
         |  }.recover(???)
         |}""".stripMargin
    )
    checkTree(defn)(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(tfunc(tparam("n", "Int"))(tapply(
        tselect(
          blk(Term.ForYield(
            List(
              Enumerator.Generator(patwildcard, tapply(tselect("scala", "util", "Success"), int(123)))
            ),
            int(42)
          )),
          "recover"
        ),
        tname("???")
      )))
    ))
  }

  test("def f[A][B]: A = ???") {
    runTestError[Stat](
      "def f[A][B]: A = ???",
      """|error: `=` expected but `[` found
         |def f[A][B]: A = ???
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B = ???") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B]: B = ???") {
      Defn.Def(
        Nil,
        tname("f"),
        List(
          pcg(List(pparam("A")), List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A"))))),
          pcg(List(pparam("B")))
        ),
        Some(pname("B")),
        tname("???")
      )
    }
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B = ???") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B = ???",
      """|error: `=` expected but `[` found
         |def f[A](implicit a: A)[B](implicit b: B): B = ???
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???") {
      Defn.Def(
        Nil,
        tname("f"),
        pcg(List(pparam("A")), List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A"))))) ::
          pcg(List(pparam("B")), List(tparam("b", "B"), tparam("bs", Type.Repeated(pname("B"))))) ::
          pcg(List(pparam("C")), List(tparam(List(Mod.Implicit()), "c", "C"))) :: Nil,
        Some(pname("B")),
        tname("???")
      )
    }
  }

}
