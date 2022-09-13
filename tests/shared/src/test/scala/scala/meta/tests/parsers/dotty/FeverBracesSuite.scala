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
}
