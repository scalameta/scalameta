package scala.meta.tests
package parsers

import scala.meta._

class VarargParameterSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala213

  test("vararg parameter single argument") {
    val obj = Defn.Def(
      Nil,
      tname("obj"),
      Nil,
      List(List(tparam("f", Type.Repeated(pname("Int"))))),
      Some(pname("Boolean")),
      bool(true)
    )
    check("def obj(f: Int*): Boolean = true")(obj)
  }

  test("vararg parameter multiple arguments") {
    val obj = Defn.Def(
      Nil,
      tname("obj"),
      Nil,
      List(List(tparam("a", "String"), tparam("b", "Boolean"), tparam("f", Type.Repeated(pname("Int"))))),
      Some(pname("Boolean")),
      bool(true)
    )
    check("def obj(a: String, b: Boolean, f: Int*): Boolean = true")(obj)
  }

  test("vararg parameter partially applied arguments") {
    val obj = Defn.Def(
      Nil,
      tname("obj"),
      Nil,
      List(
        List(tparam("fa", Type.Repeated(pname("Int")))),
        List(tparam("fb", Type.Repeated(pname("Int"))))
      ),
      Some(pname("Boolean")),
      bool(true)
    )
    check("def obj(fa: Int*)(fb: Int*): Boolean = true")(obj)
  }

  test("vararg parameter implicit argument") {
    val obj = Defn.Def(
      Nil,
      tname("obj"),
      Nil,
      List(
        List(tparam("fa", Type.Repeated(pname("Int")))),
        List(tparam(List(Mod.Implicit()), "fb", Type.Repeated(pname("Int"))))
      ),
      Some(pname("Boolean")),
      bool(true)
    )
    check("def obj(fa: Int*)(implicit fb: Int*): Boolean = true")(obj)
  }

  test("error on return type vararg parameters")(
    checkError("def obj(f: Int*): Boolean* = true", "error: `=` expected but `identifier` found")
  )

  test("error on multiple parameters vararg not last")(
    checkError("def obj(fa: Int*, fb: String): Boolean = true", "error: *-parameter must come last")
  )

  test("error on repeated byname parameter") {
    checkError("def fx(x: => Int*): Int = 3", "`)` expected but `identifier` found")
    checkError("class Foo(bars: => Int*)", "`)` expected but `identifier` found")
  }

  test("error on multiple vararg parameters")(
    checkError("def obj(fa: Int*, fb: Int*) = true", "error: *-parameter must come last")
  )

  test("vararg-like parameters") {
    checkError("def obj(fa: Int, fb: Int`*`) = true", "error: `identifier` expected but `)` found")
    checkError("def obj(fa: Int`*`, fb: Int) = true", "error: `identifier` expected but `,` found")
  }

  private def check(definition: String)(expected: scala.meta.Stat)(implicit
      loc: munit.Location
  ): Unit = checkTree(templStat(definition))(expected)

  private def checkError(definition: String, expected: String)(implicit loc: munit.Location): Unit = {
    val error = intercept[parsers.ParseException](templStat(definition))
    val obtained = error.getMessage
    assert(obtained.contains(expected), s"got: [$obtained]")
  }

  test("vararg function-type parameter") {
    val code = "def b(x: Int => List[Int]*): Int = 2"
    val tree = Defn.Def(
      Nil,
      "b",
      Nil,
      List(List(tparam("x", Type.Repeated(pfunc("Int")(papply("List", "Int")))))),
      Some("Int"),
      lit(2)
    )
    check(code)(tree)
  }

  test("#4492 with varargs") {
    val code2 = "method1((_: Seq[Int]): _*)"
    val code3 = "method1((_: Seq[Int])*)"
    val tree = tapply(
      "method1",
      Term.Repeated(Term.AnonymousFunction(Term.Ascribe(Term.Placeholder(), papply("Seq", "Int"))))
    )
    locally {
      implicit val dialect: Dialect = dialects.Scala213
      runTestAssert[Stat](code2)(tree)
      val layout = "method1((_: Seq[Int]) `*`)"
      val tree3 = tapply(
        "method1",
        Term.AnonymousFunction(tpostfix(Term.Ascribe(Term.Placeholder(), papply("Seq", "Int")), "*"))
      )
      runTestAssert[Stat](code3, layout)(tree3)
    }
    locally {
      implicit val dialect: Dialect = dialects.Scala3
      runTestAssert[Stat](code2, code3)(tree)
      runTestAssert[Stat](code3)(tree)
    }
  }

}
