package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.tests.tokenizers.TokenizerSuite

class FeverBracesSuite extends BaseDottySuite {

  override implicit val dialect = dialects.Scala3.withAllowFewerBraces(true)

  test("simple") {
    runTestAssert[Stat](
      """|val firstLine = files.get(fileName).fold:
         |    val fileNames = files.values
         |    filenames
         |
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.get(fileName).fold {
           |  val fileNames = files.values
           |  filenames
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(
            Term.Apply(
              Term.Select(Term.Name("files"), Term.Name("get")),
              List(Term.Name("fileName"))
            ),
            Term.Name("fold")
          ),
          List(
            Term.Block(
              List(
                Defn.Val(
                  Nil,
                  List(Pat.Var(Term.Name("fileNames"))),
                  None,
                  Term.Select(Term.Name("files"), Term.Name("values"))
                ),
                Term.Name("filenames")
              )
            )
          )
        )
      )
    )
  }

  test("simple-same-line") {
    runTestAssert[Stat](
      """|val firstLine = files.map: a =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map(a => a)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("files"), Term.Name("map")),
          List(Term.Function(List(Term.Param(Nil, Term.Name("a"), None, None)), Term.Name("a")))
        )
      )
    )
  }

  test("advanced-same-line") {
    runTestAssert[Stat](
      """|val firstLine = files.map: (a, b) =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map((a, b) => a)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("files"), Term.Name("map")),
          List(
            Term.Function(
              List(
                Term.Param(Nil, Term.Name("a"), None, None),
                Term.Param(Nil, Term.Name("b"), None, None)
              ),
              Term.Name("a")
            )
          )
        )
      )
    )
  }

  test("advanced-same-line-case") {
    runTestAssert[Stat](
      """|val firstLine = files.map: 
         |  case (a, b) =>
         |    a
         |""".stripMargin,
      assertLayout = Some(
        """|val firstLine = files.map({
           |  case (a, b) => a
           |})
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("firstLine"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("files"), Term.Name("map")),
          List(
            Term.PartialFunction(
              List(
                Case(
                  Pat.Tuple(List(Pat.Var(Term.Name("a")), Pat.Var(Term.Name("b")))),
                  None,
                  Term.Name("a")
                )
              )
            )
          )
        )
      )
    )
  }

  test("multiple") {
    runTestAssert[Stat](
      """|def O =
         |  val firstLine = files.fold:
         |    123
         |  val secondLine = files.fold:
         |    123
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def O = {
           |  val firstLine = files.fold(123)
           |  val secondLine = files.fold(123)
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("firstLine"))),
              None,
              Term.Apply(Term.Select(Term.Name("files"), Term.Name("fold")), List(Lit.Int(123)))
            ),
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("secondLine"))),
              None,
              Term.Apply(Term.Select(Term.Name("files"), Term.Name("fold")), List(Lit.Int(123)))
            )
          )
        )
      )
    )
  }

  test("infix") {
    runTestAssert[Stat](
      """|def O =
         |  credentials `++`:
         |    val file = Path.userHome / ".credentials"
         |    file
         |""".stripMargin,
      assertLayout = Some(
        """|def O = credentials.++ {
           |  val file = Path.userHome / ".credentials"
           |  file
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Apply(
          Term.Select(Term.Name("credentials"), Term.Name("++")),
          List(
            Term.Block(
              List(
                Defn.Val(
                  Nil,
                  List(Pat.Var(Term.Name("file"))),
                  None,
                  Term.ApplyInfix(
                    Term.Select(Term.Name("Path"), Term.Name("userHome")),
                    Term.Name("/"),
                    Nil,
                    List(Lit.String(".credentials"))
                  )
                ),
                Term.Name("file")
              )
            )
          )
        )
      )
    )
  }

  test("multiple-apply") {
    runTestAssert[Stat](
      """|def O =
         |  val firstLine = files.fold:
         |    123
         |  .apply: 
         |   (a,b) =>
         |      a
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def O = {
           |  val firstLine = files.fold(123).apply((a, b) => a)
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("firstLine"))),
              None,
              Term.Apply(
                Term.Select(
                  Term
                    .Apply(Term.Select(Term.Name("files"), Term.Name("fold")), List(Lit.Int(123))),
                  Term.Name("apply")
                ),
                List(
                  Term.Function(
                    List(
                      Term.Param(Nil, Term.Name("a"), None, None),
                      Term.Param(Nil, Term.Name("b"), None, None)
                    ),
                    Term.Name("a")
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("nested") {
    runTestAssert[Stat](
      """|def O =
         |  List((1, (List(""), 3))).map: (a: (Int, (List[String], Int))) =>
         |    a._1 + 1
         |""".stripMargin,
      assertLayout = Some(
        """|def O = List((1, (List(""), 3))).map((a: (Int, (List[String], Int))) => a._1 + 1)
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("O"),
        Nil,
        Nil,
        None,
        Term.Apply(
          Term.Select(
            Term.Apply(
              Term.Name("List"),
              List(
                Term.Tuple(
                  List(
                    Lit.Int(1),
                    Term.Tuple(
                      List(Term.Apply(Term.Name("List"), List(Lit.String(""))), Lit.Int(3))
                    )
                  )
                )
              )
            ),
            Term.Name("map")
          ),
          List(
            Term.Function(
              List(
                Term.Param(
                  Nil,
                  Term.Name("a"),
                  Some(
                    Type.Tuple(
                      List(
                        Type.Name("Int"),
                        Type.Tuple(
                          List(
                            Type.Apply(Type.Name("List"), List(Type.Name("String"))),
                            Type.Name("Int")
                          )
                        )
                      )
                    )
                  ),
                  None
                )
              ),
              Term.ApplyInfix(
                Term.Select(Term.Name("a"), Term.Name("_1")),
                Term.Name("+"),
                Nil,
                List(Lit.Int(1))
              )
            )
          )
        )
      )
    )
  }

  test("chain") {
    runTestAssert[Stat](
      """|val a: Int = xs
         |    .map: x =>
         |      x * x
         |    .filter: (y: Int) =>
         |      y > 0
         |    (0)
         |""".stripMargin,
      assertLayout = Some(
        """|val a: Int = xs.map(x => x * x).filter((y: Int) => y > 0)(0)
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("a"))),
        Some(Type.Name("Int")),
        Term.Apply(
          Term.Apply(
            Term.Select(
              Term.Apply(
                Term.Select(Term.Name("xs"), Term.Name("map")),
                List(
                  Term.Function(
                    List(Term.Param(Nil, Term.Name("x"), None, None)),
                    Term.ApplyInfix(Term.Name("x"), Term.Name("*"), Nil, List(Term.Name("x")))
                  )
                )
              ),
              Term.Name("filter")
            ),
            List(
              Term.Function(
                List(Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)),
                Term.ApplyInfix(Term.Name("y"), Term.Name(">"), Nil, List(Lit.Int(0)))
              )
            )
          ),
          List(Lit.Int(0))
        )
      )
    )
  }
}
