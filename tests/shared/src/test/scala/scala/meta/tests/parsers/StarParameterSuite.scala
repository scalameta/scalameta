package scala.meta.tests
package parsers

import scala.meta._, Defn.Class
import scala.meta.dialects.Scala211

class StarParameterSuite extends ParseSuite {

  test("star parameter single argument") {
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

  test("star parameter multiple arguments") {
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

  test("star parameter partially applied arguments") {
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

  test("star parameter implicit argument") {
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

  test("error on return type star parameters") {
    val definition = "def obj(f: Int*): Boolean* = true"

    val error = intercept[parsers.ParseException] {
      source(s"object Test { ${definition} }")
    }
    assert(error.getMessage.contains("error: = expected but identifier found"))
  }

  // TODO: This case should be correct in dotty(no error)!
  test("error on star parameter by name") {
    val definition = "def obj(f: => Int*): Boolean = true"

    val error = intercept[parsers.ParseException] {
      source(s"object Test { ${definition} }")
    }
    assert(error.getMessage.contains("error: ) expected but identifier found"))
  }

  test("error on multiple star parameters1") {
    val definition = "def obj(fa: Int*, fb: String): Boolean = true"

    val error = intercept[parsers.ParseException] {
      source(s"object Test { ${definition} }")
    }
    assert(error.getMessage.contains("error: *-parameter must come last"))
  }

  test("error on multiple star parameters2") {
    val definition = "def obj(fa: Int*, fb: Int*) = true"

    val error = intercept[parsers.ParseException] {
      source(s"object Test { ${definition} }")
    }
    assert(error.getMessage.contains("error: *-parameter must come last"))
  }

  private def check(definition: String, expected: scala.meta.Stat): Unit = {
    val wrappedExpected: scala.meta.Source = Source(
      List(
        Defn
          .Object(Nil, Term.Name("Test"), Template(Nil, Nil, Self(Name(""), None), List(expected)))
      )
    )
    assertEquals(source(s"object Test { ${definition} }"), wrappedExpected)
  }
}
