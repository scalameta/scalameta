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
        Nil,
        cparam,
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("modifier-method") {
    runTestAssert[Stat]("extension (c: Circle) private def crc: Int = 2")(
      Defn.ExtensionGroup(
        Nil,
        cparam,
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
  }

  test("simple-method-indent") {
    val code = """|extension (c: Circle)
                  |  def crc: Int = 2
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("extension (c: Circle) def crc: Int = 2"))(
      Defn.ExtensionGroup(
        Nil,
        cparam,
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("modifier-method-indent") {
    val code = """|extension (c: Circle)
                  |  private def crc: Int = 2
                  |""".stripMargin
    runTestAssert[Stat](
      code,
      assertLayout = Some("extension (c: Circle) private def crc: Int = 2")
    )(
      Defn.ExtensionGroup(
        Nil,
        List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("Circle")), None))),
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
  }

  test("multiple-methods-indent") {
    val code = """|extension (c: Circle)
                  |  def cra: Int = 2
                  |  def crb: String = "3"
                  |  def crc: Boolean = 4
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.ExtensionGroup(
        Nil,
        List(List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("Circle")), None))),
        Term.Block(
          List(
            Defn.Def(Nil, Term.Name("cra"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
            Defn.Def(Nil, Term.Name("crb"), Nil, Nil, Some(Type.Name("String")), Lit.String("3")),
            Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Boolean")), Lit.Int(4))
          )
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
        Nil,
        cparam,
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("extension-using-single") {
    val code = """|extension (c: Circle)(using Context, x: Int) {
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    val output = "extension (c: Circle)(using Context, x: Int) def crc: Int = 2"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.ExtensionGroup(
        Nil,
        List(
          List(Term.Param(Nil, Term.Name("c"), Some(pname("Circle")), None)),
          List(
            Term.Param(List(Mod.Using()), Name.Anonymous(), Some(pname("Context")), None),
            Term.Param(List(Mod.Using()), Term.Name("x"), Some(pname("Int")), None)
          )
        ),
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("extension-using-newline") {
    val code = """|extension (c: Circle)
                  |  (using Context, x: Int) 
                  |{
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    val output = "extension (c: Circle)(using Context, x: Int) def crc: Int = 2"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.ExtensionGroup(
        Nil,
        List(
          List(Term.Param(Nil, Term.Name("c"), Some(pname("Circle")), None)),
          List(
            Term.Param(List(Mod.Using()), Name.Anonymous(), Some(pname("Context")), None),
            Term.Param(List(Mod.Using()), Term.Name("x"), Some(pname("Int")), None)
          )
        ),
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("extension-using-multi") {
    val code = """|extension (c: Circle)(using Context, x: Int)(using y: String, File) {
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    val output =
      "extension (c: Circle)(using Context, x: Int)(using y: String, File) def crc: Int = 2"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.ExtensionGroup(
        Nil,
        List(
          List(Term.Param(Nil, Term.Name("c"), Some(Type.Name("Circle")), None)),
          List(
            Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("Context")), None),
            Term.Param(List(Mod.Using()), Term.Name("x"), Some(Type.Name("Int")), None)
          ),
          List(
            Term.Param(List(Mod.Using()), Term.Name("y"), Some(Type.Name("String")), None),
            Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("File")), None)
          )
        ),
        Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
      )
    )
  }

  test("extension-additional-comment") {
    runTestAssert[Stat](
      """|extension (a: Int)
         |
         |
         |    /** */
         |
         |
         |    def double = a * 2
         |    
         |    /** */
         |""".stripMargin,
      assertLayout = Some("extension (a: Int) def double = a * 2")
    )(
      Defn.ExtensionGroup(
        Nil,
        List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))),
        Defn.Def(
          Nil,
          Term.Name("double"),
          Nil,
          Nil,
          None,
          Term.ApplyInfix(Term.Name("a"), Term.Name("*"), Nil, List(Lit.Int(2)))
        )
      )
    )
  }

  // Scala 3 doesn't allow for methods named `extension` to be invoked
  // https://github.com/lampepfl/dotty/issues/10076
  test("extension-named-method") {
    runTestError[Stat](
      """|object A{
         |  def extension(a : Int) = a + 2
         |  extension(2)
         |}""".stripMargin,
      "identifier expected but integer constant found"
    )

    runTestAssert[Stat](
      """|object A {
         |  def extension(a: Int) = a + 2
         |  `extension`(2)
         |}""".stripMargin
    )(
      Defn.Object(
        Nil,
        Term.Name("A"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(
              Nil,
              Term.Name("extension"),
              Nil,
              List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))),
              None,
              Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Lit.Int(2)))
            ),
            Term.Apply(Term.Name("extension"), List(Lit.Int(2)))
          ),
          Nil
        )
      )
    )
  }

  test("extension-named-method") {
    runTestAssert[Stat](
      "extension + 3"
    )(
      Term.ApplyInfix(Term.Name("extension"), Term.Name("+"), Nil, List(Lit.Int(3)))
    )

    runTestAssert[Stat](
      "def extension(x: extension): extension = x"
    )(
      Defn.Def(
        Nil,
        Term.Name("extension"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("extension")), None))),
        Some(Type.Name("extension")),
        Term.Name("x")
      )
    )

    runTestAssert[Stat](
      "extension.extension(3)"
    )(
      Term.Apply(Term.Select(Term.Name("extension"), Term.Name("extension")), List(Lit.Int(3)))
    )
  }

  test("method-type-params") {
    runTestAssert[Stat](
      "extension [T](xs: List[T]) def sumBy[U](t: T): U = ???"
    )(
      Defn.ExtensionGroup(
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("xs"),
              Some(Type.Apply(Type.Name("List"), List(Type.Name("T")))),
              None
            )
          )
        ),
        Defn.Def(
          Nil,
          Term.Name("sumBy"),
          List(Type.Param(Nil, Type.Name("U"), Nil, Type.Bounds(None, None), Nil, Nil)),
          List(List(Term.Param(Nil, Term.Name("t"), Some(Type.Name("T")), None))),
          Some(Type.Name("U")),
          Term.Name("???")
        )
      )
    )
  }

  test("method-using-before") {
    runTestAssert[Stat](
      "extension (using a: Int)(b: Int) def hello = a + b"
    )(
      Defn.ExtensionGroup(
        Nil,
        List(
          List(
            Term.Param(List(Mod.Using()), Term.Name("a"), Some(Type.Name("Int")), None)
          ),
          List(Term.Param(Nil, Term.Name("b"), Some(Type.Name("Int")), None))
        ),
        Defn.Def(
          Nil,
          Term.Name("hello"),
          Nil,
          Nil,
          None,
          Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b")))
        )
      )
    )
  }

  test("method-multi-using") {
    runTestAssert[Stat](
      """|extension 
         |  (
         |    using a: Int
         |  )
         |  (
         |    b: Int
         |  )
         |  (
         |    using c: String
         |  ) 
         |    def hello = a + b + c.toInt""".stripMargin,
      assertLayout =
        Some("extension (using a: Int)(b: Int)(using c: String) def hello = a + b + c.toInt")
    )(
      Defn.ExtensionGroup(
        Nil,
        List(
          List(Term.Param(List(Mod.Using()), Term.Name("a"), Some(Type.Name("Int")), None)),
          List(Term.Param(Nil, Term.Name("b"), Some(Type.Name("Int")), None)),
          List(Term.Param(List(Mod.Using()), Term.Name("c"), Some(Type.Name("String")), None))
        ),
        Defn.Def(
          Nil,
          Term.Name("hello"),
          Nil,
          Nil,
          None,
          Term.ApplyInfix(
            Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))),
            Term.Name("+"),
            Nil,
            List(Term.Select(Term.Name("c"), Term.Name("toInt")))
          )
        )
      )
    )
  }

  final val defcrc = Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))

  final val cparam = List(List(tparam("c", "Circle")))
}
