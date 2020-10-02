package scala.meta.tests
package parsers

import scala.meta._

class VarargParameterSuite extends ParseSuite {

  import scala.meta.dialects.Scala213

  test("vararg parameter single argument") {
    val obj = Defn.Def(
      Nil,
      Term.Name("obj"),
      Nil,
      List(
        List(
          Term.Param(
            Nil,
            Term.Name("f"),
            Some(Type.Repeated(Type.Name("Int"))),
            None
          )
        )
      ),
      Some(Type.Name("Boolean")),
      Lit.Boolean(true)
    )
    check("def obj(f: Int*): Boolean = true", obj)
  }

  test("vararg parameter multiple arguments") {
    val obj = Defn.Def(
      Nil,
      Term.Name("obj"),
      Nil,
      List(
        List(
          Term.Param(Nil, Term.Name("a"), Some(Type.Name("String")), None),
          Term.Param(Nil, Term.Name("b"), Some(Type.Name("Boolean")), None),
          Term.Param(
            Nil,
            Term.Name("f"),
            Some(Type.Repeated(Type.Name("Int"))),
            None
          )
        )
      ),
      Some(Type.Name("Boolean")),
      Lit.Boolean(true)
    )
    check("def obj(a: String, b: Boolean, f: Int*): Boolean = true", obj)
  }

  test("vararg parameter partially applied arguments") {
    val obj = Defn.Def(
      Nil,
      Term.Name("obj"),
      Nil,
      List(
        List(Term.Param(Nil, Term.Name("fa"), Some(Type.Repeated(Type.Name("Int"))), None)),
        List(Term.Param(Nil, Term.Name("fb"), Some(Type.Repeated(Type.Name("Int"))), None))
      ),
      Some(Type.Name("Boolean")),
      Lit.Boolean(true)
    )
    check("def obj(fa: Int*)(fb: Int*): Boolean = true", obj)
  }

  test("vararg parameter implicit argument") {
    val obj = Defn.Def(
      Nil,
      Term.Name("obj"),
      Nil,
      List(
        List(Term.Param(Nil, Term.Name("fa"), Some(Type.Repeated(Type.Name("Int"))), None)),
        List(
          Term.Param(
            List(Mod.Implicit()),
            Term.Name("fb"),
            Some(Type.Repeated(Type.Name("Int"))),
            None
          )
        )
      ),
      Some(Type.Name("Boolean")),
      Lit.Boolean(true)
    )
    check("def obj(fa: Int*)(implicit fb: Int*): Boolean = true", obj)
  }

  test("error on return type vararg parameters") {
    checkError(
      "def obj(f: Int*): Boolean* = true",
      "error: = expected but identifier found"
    )
  }

  test("error on multiple parameters vararg not last") {
    checkError(
      "def obj(fa: Int*, fb: String): Boolean = true",
      "error: *-parameter must come last"
    )
  }

  test("error on repeated byname parameter") {
    checkError(
      "def fx(x: => Int*): Int = 3",
      ") expected but identifier found"
    )
    checkError(
      "class Foo(bars: => Int*)",
      ") expected but identifier found"
    )
  }

  test("error on multiple vararg parameters") {
    checkError(
      "def obj(fa: Int*, fb: Int*) = true",
      "error: *-parameter must come last"
    )
  }

  private def check(definition: String, expected: scala.meta.Stat): Unit = {
    val obtained = templStat(definition)
    assertNoDiff(obtained.structure, expected.structure)
    assertNoDiff(obtained.syntax, expected.syntax)
  }

  private def checkError(definition: String, expected: String): Unit = {
    val error = intercept[parsers.ParseException] {
      templStat(definition)
    }
    assert(clue(error).getMessage.contains(clue(expected)))
  }

}
