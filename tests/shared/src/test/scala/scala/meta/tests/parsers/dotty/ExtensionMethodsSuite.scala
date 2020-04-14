package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ExtensionMethodsSuite extends ParseSuite {
  
  /** For checking examples in repl declare:
   *  case class Circle(x: Int)
   * 
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html
   */

  // ---------------------------------
  // EXTENSION METHOD
  // ---------------------------------

  test("simple") {
    List(
      "def (c: Circle).circumference: Int = 2",
      "def (c: Circle) circumference: Int = 2"
    ).foreach { code =>
      runTestAssert(code)(
        Defn.ExtensionMethod(Nil, cparam, Term.Name("circumference"), Nil, Nil,
          Some(Type.Name("Int")), Lit.Int(2))
      )
    }
  }
  
  test("no-return-type") {
    runTestAssert("def (c: Circle) circumference = 2")(
      Defn.ExtensionMethod(Nil, cparam, Term.Name("circumference"), Nil, Nil,
        None, Lit.Int(2))
    )
  }

  test("with-parameters") {
    List(
      "def (c: Circle).circumference(a: Int)(b: String): Int = 2",
      "def (c: Circle) circumference(a: Int)(b: String): Int = 2"
    ).foreach { code =>
      runTestAssert(code)(
        Defn.ExtensionMethod(Nil, cparam, Term.Name("circumference"), Nil,
         List(List(termParam("a", "Int")), List(termParam("b", "String"))), Some(Type.Name("Int")), Lit.Int(2))
      )
    }
  }

  test("with-using-parameters") {
    runTestAssert("def (c: Circle).circumference(using s: String)(using Int): Int = 2")(
        Defn.ExtensionMethod(Nil, cparam, Term.Name("circumference"), Nil, List(
          List(Term.Param(List(Mod.Using()), Term.Name("s"), Some(Type.Name("String")), None)),
          List(Term.Param(List(Mod.Using()), meta.Name.Anonymous(), Some(Type.Name("Int")), None))
          ), Some(Type.Name("Int")), Lit.Int(2))
    )
  }

  test("with-rhs-block") {
    val rhs = Term.Block(List(Defn.Val(
      Nil, List(Pat.Var(Term.Name("p"))),
      None, Lit.Int(314)),
    Term.Select(Term.Name("c"),
      Term.Name("x"))))

    runTestAssert("def (c: Circle) circumference: Int = { val p = 314; c.x }")(
      Defn.ExtensionMethod(Nil, cparam, Term.Name("circumference"), Nil,
        Nil, Some(Type.Name("Int")), rhs)
    )
  }

  test("generics") {
    val objTpe = Term.Param(Nil, Term.Name("xs"), Some(Type.Apply(Type.Name("List"), List(
      Type.Placeholder(Type.Bounds(None, Some(Type.Name("T"))))))), None)
    val tTpe = Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, List(Type.Name("Ord")))

    runTestAssert("def [T : Ord](xs: List[_ <: T]).second = 2")(
      Defn.ExtensionMethod(Nil, objTpe, Term.Name("second"), List(tTpe),
        Nil, None, Lit.Int(2))
    )
  }

  test("operators") {
    runTestAssert("def (x: String) < (y: String) = 2")(
      Defn.ExtensionMethod(Nil, termParam("x", "String"), Term.Name("<"), Nil,
        List(List(termParam("y", "String"))), None, Lit.Int(2))
    )
    runTestAssert("def (x: String) +: (y: String) = 2")(
      Defn.ExtensionMethod(Nil, termParam("x", "String"), Term.Name("+:"), Nil,
        List(List(termParam("y", "String"))), None, Lit.Int(2))
    )
  }

  // ---------------------------------
  // EXTENSION GROUP
  // ---------------------------------

  test("extension-anonymous") {
    runTestAssert("extension { \n def (c1: Circle).cf1: Int = 2 \n def (c2: Circle).cf2: Int = 2}")(
      Defn.ExtensionGroup(Nil, meta.Name.Anonymous(), Nil, Nil,
        Term.Param(Nil, meta.Name.Anonymous(), None, None),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
        circleExtMethod("c1", "cf1"),
        circleExtMethod("c2", "cf2")
      )))
    )
  }

  test("extension-named") {
    runTestAssert("extension ext { \n def (c1: Circle).cf1: Int = 2 \n def (c2: Circle).cf2: Int = 2}")(
      Defn.ExtensionGroup(Nil, Type.Name("ext"), Nil, Nil,
        Term.Param(Nil, meta.Name.Anonymous(), None, None),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
        circleExtMethod("c1", "cf1"),
        circleExtMethod("c2", "cf2")
      )))
    )
  }

  test("extension-on") {
    runTestAssert("extension on (c: Circle) { \n def cf1: Int = 2 \n def cf2: Int = 2}")(
      Defn.ExtensionGroup(Nil, meta.Name.Anonymous(), Nil, Nil,
          termParam("c", "Circle"),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
        circleAnonExtMethod("cf1"),
        circleAnonExtMethod("cf2")
      )))
    )
    runTestAssert("extension ext on (c: Circle) { \n def cf1: Int = 2 \n def cf2: Int = 2}")(
      Defn.ExtensionGroup(Nil, Type.Name("ext"), Nil, Nil,
          termParam("c", "Circle"),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
        circleAnonExtMethod("cf1"),
        circleAnonExtMethod("cf2")
      )))
    )
  }

  test("extension-generis-group") {
    val tTpe = Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, List(Type.Name("Ord")))
    runTestAssert("extension ext on [T : Ord](c: Circle) { def f: Int = 2 }")(
      Defn.ExtensionGroup(Nil, Type.Name("ext"), List(tTpe), Nil,
          termParam("c", "Circle"),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
            circleAnonExtMethod("f")
      )))
    )
  }

  test("extension-negative") {
    runTestError("extension { }", "Anonymous instance must have at least one extension method")
    runTestError("extension on (s: String) { }", "Anonymous instance must have at least one extension method")

    runTestError("extension { def (s: String).cnt: Int = 2; val x = 3 }",
     "Extension clause can only define methods")

    runTestError("extension on (c: Circle) { def (a: Int).f: Int = 2 }", "no extension method allowed here since leading parameter was already given")

    runTestError("extension on [T](c: Circle) { def f[X](a: Int): Int = 2 }", "extension method cannot have type parameters since some were already given previously")
  }

  test("extension-on-using") {
    runTestAssert("extension ext on (c: Circle)(using a: Int)(using String) { def f: Int = 2 }")(
      Defn.ExtensionGroup(Nil, Type.Name("ext"), Nil, List(
        List(Term.Param(List(Mod.Using()), Term.Name("a"), Some(Type.Name("Int")), None)),
        List(Term.Param(List(Mod.Using()), meta.Name.Anonymous(), Some(Type.Name("String")), None))
      ), termParam("c", "Circle"),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
        circleAnonExtMethod("f")
      )))
    )
  }

  test("extension-selftype") {
    runTestAssert("extension ext on (c: Circle) { slf => def f: Int = 2 }")(
      Defn.ExtensionGroup(Nil, Type.Name("ext"), Nil, Nil,
          termParam("c", "Circle"),
          Template(Nil, Nil, meta.Self(Term.Name("slf"), None), List(
        circleAnonExtMethod("f")
      )))
    )
  }

  test("extension-mods".ignore) {
    runTestAssert("private extension ext { def (c: Circle).f(using s: String)(using Int): Int = 2 }")(
      Defn.ExtensionGroup(List(Mod.Lazy()), Type.Name("ext"), Nil, Nil,
          Term.Param(Nil, meta.Name.Anonymous(), None, None),
          Template(Nil, Nil, meta.Self(meta.Name.Anonymous(), None), List(
            Defn.ExtensionMethod(Nil, termParam("c", "Circle"), Term.Name("f"), Nil, List(
              List(Term.Param(List(Mod.Using()), Term.Name("s"), Some(Type.Name("String")), None)),
              List(Term.Param(List(Mod.Using()), meta.Name.Anonymous(), Some(Type.Name("Int")), None)),
            ), Some(Type.Name("Int")), Lit.Int(2))
      )))
    )
  }

  // extreme examples :)
  //
  // object X { def (c: Circle).circumference: Int = 2 }  // must be defined
  // trait X { def (c: Circle).circumference: Int }       // rhs can be omitted
  // inline def [T](c: Circle)f(x: T) <: T = x
  // def [T](c: Circle)f(x: T): T{val x: String} = ???

  def circleExtMethod(on: String, name: String): Defn.ExtensionMethod =
    Defn.ExtensionMethod(Nil, termParam(on, "Circle"), Term.Name(name), Nil,
      List(), Some(Type.Name("Int")), Lit.Int(2))

  def circleAnonExtMethod(name: String): Defn.Def =
    Defn.Def(Nil, Term.Name(name), Nil,
      Nil, Some(Type.Name("Int")), Lit.Int(2))

  def termParam(name: String, tpe: String): Term.Param =
    Term.Param(Nil, Term.Name(name), Some(Type.Name(tpe)), None)

  val cparam = termParam("c", "Circle")
   
  private def runTestAssert(code: String)(expected: Stat) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val obtained: Stat = blockStat(code)
    try {
      assertEquals(obtained, expected)
    } catch {
      case e: Throwable =>
        println(s"Generated stat: \n ${obtained.structure}")
        throw e
    }
  }

  private def runTestError(code: String, expected: String) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val error = intercept[ParseException] {
      val result = blockStat(code)
      println(s"Statement ${code} should not parse! Got result ${result.structure}")
    }
    if (!error.getMessage().contains(expected)) {
      println(s"Expected [${error.getMessage}] to contain [${expected}].")
    }
    assert(error.getMessage.contains(expected))
  }
}
