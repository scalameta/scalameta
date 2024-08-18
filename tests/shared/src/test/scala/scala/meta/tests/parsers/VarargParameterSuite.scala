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
    check("def obj(f: Int*): Boolean = true", obj)
  }

  test("vararg parameter multiple arguments") {
    val obj = Defn.Def(
      Nil,
      tname("obj"),
      Nil,
      List(
        List(tparam("a", "String"), tparam("b", "Boolean"), tparam("f", Type.Repeated(pname("Int"))))
      ),
      Some(pname("Boolean")),
      bool(true)
    )
    check("def obj(a: String, b: Boolean, f: Int*): Boolean = true", obj)
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
    check("def obj(fa: Int*)(fb: Int*): Boolean = true", obj)
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
    check("def obj(fa: Int*)(implicit fb: Int*): Boolean = true", obj)
  }

  test("error on return type vararg parameters") {
    checkError("def obj(f: Int*): Boolean* = true", "error: `=` expected but `identifier` found")
  }

  test("error on multiple parameters vararg not last") {
    checkError("def obj(fa: Int*, fb: String): Boolean = true", "error: *-parameter must come last")
  }

  test("error on repeated byname parameter") {
    checkError("def fx(x: => Int*): Int = 3", "`)` expected but `identifier` found")
    checkError("class Foo(bars: => Int*)", "`)` expected but `identifier` found")
  }

  test("error on multiple vararg parameters") {
    checkError("def obj(fa: Int*, fb: Int*) = true", "error: *-parameter must come last")
  }

  test("vararg-like parameters") {
    checkError("def obj(fa: Int, fb: Int`*`) = true", "error: `identifier` expected but `)` found")
    checkError("def obj(fa: Int`*`, fb: Int) = true", "error: `identifier` expected but `,` found")
  }

  private def check(definition: String, expected: scala.meta.Stat): Unit =
    checkTree(templStat(definition))(expected)

  private def checkError(definition: String, expected: String): Unit = {
    val error = intercept[parsers.ParseException](templStat(definition))
    val obtained = error.getMessage
    assert(obtained.contains(expected), s"got: [$obtained]")
  }

}
