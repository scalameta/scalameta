package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ExtensionMethodsSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)

  /**
   * For checking examples in repl declare:
   *  case class Circle(x: Int)
   *
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html
   */
  // ---------------------------------
  // EXTENSION METHOD
  // ---------------------------------

  test("simple-method") {
    runTestAssert[Stat]("def (c: Circle).circumference: Int = 2")(
      Defn.ExtensionMethod(
        Nil,
        cparam,
        tname("circumference"),
        Nil,
        Nil,
        Some(pname("Int")),
        int(2)
      )
    )
  }

  test("simple-method-infix") {
    runTestAssert[Stat]("def (c: Circle) circumference: Int = 2")(
      Defn.ExtensionMethodInfix(
        Nil,
        cparam,
        tname("circumference"),
        Nil,
        Nil,
        Some(pname("Int")),
        int(2)
      )
    )
  }

  test("no-return-type") {
    runTestAssert[Stat]("def (c: Circle) circumference = 2")(
      Defn.ExtensionMethodInfix(Nil, cparam, tname("circumference"), Nil, Nil, None, int(2))
    )
  }

  test("with-parameters") {
    runTestAssert[Stat]("def (c: Circle).circumference(a: Int)(b: String): Int = 2")(
      Defn.ExtensionMethod(
        Nil,
        cparam,
        tname("circumference"),
        Nil,
        List(List(tparam("a", "Int")), List(tparam("b", "String"))),
        Some(pname("Int")),
        int(2)
      )
    )
  }

  test("with-parameters-infix") {
    runTestAssert[Stat]("def (c: Circle) circumference(a: Int)(b: String): Int = 2")(
      Defn.ExtensionMethodInfix(
        Nil,
        cparam,
        tname("circumference"),
        Nil,
        List(List(tparam("a", "Int")), List(tparam("b", "String"))),
        Some(pname("Int")),
        int(2)
      )
    )
  }

  test("with-using-parameters") {
    runTestAssert[Stat]("def (c: Circle).circumference(using s: String)(using Int): Int = 2")(
      Defn.ExtensionMethod(
        Nil,
        cparam,
        tname("circumference"),
        Nil,
        List(
          List(tparamUsing("s", "String")),
          List(tparamUsing("", "Int"))
        ),
        Some(pname("Int")),
        int(2)
      )
    )
  }

  test("with-rhs-block") {
    val rhs = Term.Block(
      List(
        Defn.Val(Nil, List(Pat.Var(tname("p"))), None, int(314)),
        Term.Select(tname("c"), tname("x"))
      )
    )

    runTestAssert[Stat]("def (c: Circle) circumference: Int = { val p = 314; c.x }", None)(
      Defn.ExtensionMethodInfix(
        Nil,
        cparam,
        tname("circumference"),
        Nil,
        Nil,
        Some(pname("Int")),
        rhs
      )
    )
  }

  test("generics") {
    val objTpe = Term.Param(
      Nil,
      tname("xs"),
      Some(Type.Apply(pname("List"), List(Type.Placeholder(Type.Bounds(None, Some(pname("T"))))))),
      None
    )
    val tTpe = Type.Param(Nil, pname("T"), Nil, Type.Bounds(None, None), Nil, List(pname("Ord")))

    runTestAssert[Stat]("def [T: Ord](xs: List[_ <: T]).second = 2")(
      Defn.ExtensionMethod(Nil, objTpe, tname("second"), List(tTpe), Nil, None, int(2))
    )
  }

  test("operators") {
    runTestAssert[Stat]("def (x: String) <(y: String) = 2")(
      Defn.ExtensionMethodInfix(
        Nil,
        tparam("x", "String"),
        tname("<"),
        Nil,
        List(List(tparam("y", "String"))),
        None,
        int(2)
      )
    )
    runTestAssert[Stat]("def (x: String) +:(y: String) = 2")(
      Defn.ExtensionMethodInfix(
        Nil,
        tparam("x", "String"),
        tname("+:"),
        Nil,
        List(List(tparam("y", "String"))),
        None,
        int(2)
      )
    )
  }

  // ---------------------------------
  // EXTENSION GROUP
  // ---------------------------------

  test("extension-anonymous") {
    runTestAssert[Stat](
      "extension {\n  def (c1: Circle).cf1: Int = 2\n  def (c2: Circle).cf2: Int = 2\n}"
    )(
      Defn.ExtensionGroup(
        Nil,
        anon,
        Nil,
        Nil,
        Term.Param(Nil, anon, None, None),
        tpl(
          List(
            circleExtMethod("c1", "cf1"),
            circleExtMethod("c2", "cf2")
          )
        )
      )
    )
  }

  test("extension-named") {
    runTestAssert[Stat](
      "extension ext {\n  def (c1: Circle).cf1: Int = 2\n  def (c2: Circle).cf2: Int = 2\n}"
    )(
      Defn.ExtensionGroup(
        Nil,
        pname("ext"),
        Nil,
        Nil,
        Term.Param(Nil, anon, None, None),
        tpl(
          List(
            circleExtMethod("c1", "cf1"),
            circleExtMethod("c2", "cf2")
          )
        )
      )
    )
  }

  test("extension-on") {
    runTestAssert[Stat]("extension on (c: Circle) {\n  def cf1: Int = 2\n  def cf2: Int = 2\n}")(
      Defn.ExtensionGroup(
        Nil,
        anon,
        Nil,
        Nil,
        tparam("c", "Circle"),
        tpl(
          List(
            circleAnonExtMethod("cf1"),
            circleAnonExtMethod("cf2")
          )
        )
      )
    )
    runTestAssert[Stat](
      "extension ext on (c: Circle) {\n  def cf1: Int = 2\n  def cf2: Int = 2\n}"
    )(
      Defn.ExtensionGroup(
        Nil,
        pname("ext"),
        Nil,
        Nil,
        tparam("c", "Circle"),
        tpl(
          List(
            circleAnonExtMethod("cf1"),
            circleAnonExtMethod("cf2")
          )
        )
      )
    )
  }

  test("extension-generis-group") {
    val tTpe = Type.Param(Nil, pname("T"), Nil, Type.Bounds(None, None), Nil, List(pname("Ord")))
    runTestAssert[Stat]("extension ext on [T: Ord](c: Circle) { def f: Int = 2 }")(
      Defn.ExtensionGroup(
        Nil,
        pname("ext"),
        List(tTpe),
        Nil,
        tparam("c", "Circle"),
        tpl(
          List(
            circleAnonExtMethod("f")
          )
        )
      )
    )
  }

  test("extension-negative") {
    runTestError(
      "extension { def (s: String).cnt: Int = 2; val x = 3 }",
      "Extension clause can only define methods"
    )

    runTestError(
      "extension on (c: Circle) { def (a: Int).f: Int = 2 }",
      "no extension method allowed here since leading parameter was already given"
    )

    runTestError(
      "extension on [T](c: Circle) { def f[X](a: Int): Int = 2 }",
      "extension method cannot have type parameters since some were already given previously"
    )
  }

  test("extension-on-using") {
    runTestAssert[Stat](
      "extension ext on (c: Circle)(using a: Int)(using String) { def f: Int = 2 }"
    )(
      Defn.ExtensionGroup(
        Nil,
        pname("ext"),
        Nil,
        List(
          List(tparamUsing("a", "Int")),
          List(tparamUsing("", "String"))
        ),
        tparam("c", "Circle"),
        tpl(
          List(
            circleAnonExtMethod("f")
          )
        )
      )
    )
  }

  test("extension-selftype") {
    runTestAssert[Stat]("extension ext on (c: Circle) { slf => def f: Int = 2 }")(
      Defn.ExtensionGroup(
        Nil,
        pname("ext"),
        Nil,
        Nil,
        tparam("c", "Circle"),
        Template(
          Nil,
          Nil,
          meta.Self(tname("slf"), None),
          List(
            circleAnonExtMethod("f")
          )
        )
      )
    )
  }

  test("extension-mods") {
    runTestAssert[Stat](
      "object X { private extension ext { def (c: Circle).f(using s: String)(using Int): Int = 2 } }"
    )(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.ExtensionGroup(
              List(Mod.Private(Name(""))),
              Type.Name("ext"),
              Nil,
              Nil,
              Term.Param(Nil, Name(""), None, None),
              Template(
                Nil,
                Nil,
                Self(Name(""), None),
                List(
                  Defn.ExtensionMethod(
                    Nil,
                    Term.Param(Nil, Term.Name("c"), Some(Type.Name("Circle")), None),
                    Term.Name("f"),
                    Nil,
                    List(
                      List(
                        Term
                          .Param(List(Mod.Using()), Term.Name("s"), Some(Type.Name("String")), None)
                      ),
                      List(Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("Int")), None))
                    ),
                    Some(Type.Name("Int")),
                    Lit.Int(2)
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  def circleExtMethod(on: String, name: String): Defn.ExtensionMethod =
    Defn.ExtensionMethod(
      Nil,
      tparam(on, "Circle"),
      tname(name),
      Nil,
      List(),
      Some(pname("Int")),
      int(2)
    )

  def circleAnonExtMethod(name: String): Defn.Def =
    Defn.Def(Nil, tname(name), Nil, Nil, Some(pname("Int")), int(2))

  final val cparam = tparam("c", "Circle")
}
