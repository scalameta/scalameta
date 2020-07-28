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

  test("simple-method") {
    runTestAssert[Stat]("extension (c: Circle) def crc: Int = 2")(
      Defn.ExtensionGroup(
        cparam,
        Nil,
        List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
      )
    )
  }

  test("modifier-method") {
    runTestAssert[Stat]("extension (c: Circle) private def crc: Int = 2")(
      Defn.ExtensionGroup(
        cparam,
        Nil,
        List(
          Defn.Def(
            List(Mod.Private(Name.Anonymous())),
            tname("crc"),
            Nil,
            Nil,
            Some(pname("Int")),
            int(2)
          )
        )
      )
    )
  }

  test("simple-method-indent") {
    val code = """|extension (c: Circle):
                  |  def crc: Int = 2
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("extension (c: Circle) def crc: Int = 2"))(
      Defn.ExtensionGroup(
        cparam,
        Nil,
        List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
      )
    )
  }

  test("modifier-method-indent") {
    val code = """|extension (c: Circle):
                  |  private def crc: Int = 2
                  |""".stripMargin
    runTestAssert[Stat](
      code,
      assertLayout = Some("extension (c: Circle) private def crc: Int = 2")
    )(
      Defn.ExtensionGroup(
        Term.Param(Nil, Term.Name("c"), Some(Type.Name("Circle")), None),
        Nil,
        List(
          Defn.Def(
            List(Mod.Private(Name(""))),
            Term.Name("crc"),
            Nil,
            Nil,
            Some(Type.Name("Int")),
            Lit.Int(2)
          )
        )
      )
    )
  }

  test("multiple-methods-indent") {
    val code = """|extension (c: Circle):
                  |  def cra: Int = 2
                  |  def crb: String = "3"
                  |  def crc: Boolean = 4
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.ExtensionGroup(
        Term.Param(Nil, Term.Name("c"), Some(Type.Name("Circle")), None),
        Nil,
        List(
          Defn.Def(Nil, Term.Name("cra"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
          Defn.Def(Nil, Term.Name("crb"), Nil, Nil, Some(Type.Name("String")), Lit.String("3")),
          Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Boolean")), Lit.Int(4))
        )
      )
    )
  }

  test("simple-method-braces") {
    val code = """|extension (c: Circle) {
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("extension (c: Circle) def crc: Int = 2"))(
      Defn.ExtensionGroup(
        cparam,
        Nil,
        List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
      )
    )
  }

  test("extension-soft-keyword") {
    runTestAssert[Stat]("val c = f(a + extension)")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("c"))),
        None,
        Term.Apply(
          Term.Name("f"),
          List(Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("extension"))))
        )
      )
    )
  }

  final val defcrc = Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))

  final val cparam = tparam("c", "Circle")
}
