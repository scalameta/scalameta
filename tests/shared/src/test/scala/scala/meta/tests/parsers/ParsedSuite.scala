package scala.meta.tests.parsers

import scala.meta._
import scala.meta.inputs.InputException
import scala.meta.parsers.{Parse, ParserOptions}
import scala.meta.tests.TreeSuiteBase
import scala.meta.tokenizers.{Tokenize, TokenizeException, Tokenized}

class ParsedSuite extends TreeSuiteBase {

  implicit val dialect: Dialect = dialects.Scala213

  private def error(parsed: Parsed[Tree]): Parsed.Error = parsed match {
    case e: Parsed.Error => e
    case x => fail(s"expected an error, got $x")
  }

  test("statement, one-argument apply") {
    val parsed = Parse.parseStat(Input.String("val a = 1"))
    assertEquals(
      parsed.get.structure,
      """|Defn.Val(
         |  Nil,
         |  List(
         |    Pat.Var(Term.Name("a"))
         |  ),
         |  None,
         |  Lit.Int(1)
         |)""".stripMargin,
    )
  }

  test("statement, two-argument apply") {
    val parsed = Parse.parseStat(Input.String("val a = 1"), dialect)
    assertEquals(
      parsed.get.structure,
      """|Defn.Val(
         |  Nil,
         |  List(
         |    Pat.Var(Term.Name("a"))
         |  ),
         |  None,
         |  Lit.Int(1)
         |)""".stripMargin,
    )
  }

  test("statement with a parse error, one-argument apply") {
    val e = error(Parse.parseStat(Input.String("val = 1")))
    assertEquals(
      (e.pos.start, e.message, e.details.getClass.getName),
      (4, "illegal start of simple pattern", "scala.meta.parsers.ParseException"),
    )
  }

  test("statement with a parse error, two-argument apply") {
    val e = error(Parse.parseStat(Input.String("val = 1"), dialect))
    assertEquals(
      (e.pos.start, e.message, e.details.getClass.getName),
      (4, "illegal start of simple pattern", "scala.meta.parsers.ParseException"),
    )
  }

  private val failingTokenize: Tokenize = new Tokenize {
    def apply(input: Input, dialect: Dialect): Tokenized = {
      val pos = Position.Range(input, 0, 1)
      Tokenized.Error(pos, "no tokens", TokenizeException(pos, "no tokens"))
    }
  }

  test("statement with a tokenizer error, one-argument apply") {
    val e = Tokenize.global.withValue(failingTokenize)(error(Parse.parseStat(Input.String("a"))))
    assertEquals(
      (e.pos.start, e.message, e.details.getClass.getName),
      (0, "no tokens", "scala.meta.tokenizers.TokenizeException"),
    )
  }

  test("statement with a tokenizer error, two-argument apply") {
    val e = Tokenize.global
      .withValue(failingTokenize)(error(Parse.parseStat(Input.String("a"), dialect)))
    assertEquals(
      (e.pos.start, e.message, e.details.getClass.getName),
      (0, "no tokens", "scala.meta.tokenizers.TokenizeException"),
    )
  }

  private class CustomInputException(input: Input)
      extends InputException(Position.Range(input, 0, 1), "custom")

  private val throwingTokenize: Tokenize = new Tokenize {
    def apply(input: Input, dialect: Dialect): Tokenized = throw new CustomInputException(input)
  }

  test("statement with a custom input exception, one-argument apply") {
    def parse() = Tokenize.global.withValue(throwingTokenize)(Parse.parseStat(Input.String("a")))
    val e = error(parse())
    assertEquals(
      (e.pos.start, e.message, e.details.getClass.getName),
      (0, "custom", classOf[CustomInputException].getName),
    )
  }

  test("statement with a custom input exception, two-argument apply") {
    def parse() = Tokenize.global
      .withValue(throwingTokenize)(Parse.parseStat(Input.String("a"), dialect))
    val e = error(parse())
    assertEquals(
      (e.pos.start, e.message, e.details.getClass.getName),
      (0, "custom", classOf[CustomInputException].getName),
    )
  }

  test("ammonite script") {
    val code =
      """|object A
         |@
         |object B
         |""".stripMargin
    val parsed = Parse.parseAmmonite(Input.Ammonite(Input.String(code)))
    assertEquals(
      parsed.get.sources.map(_.stats.map(_.syntax)),
      List(List("object A"), List("object B")),
    )
  }

  test("term with a term unquote") {
    val parsed = Parse.parseTerm(Input.String("$a + 1"), dialect.unquoteTerm(multiline = false))
    assertEquals(
      parsed.get.structure,
      """|Term.ApplyInfix(
         |  Term.Quasi(
         |    0,
         |    Term.Name("a")
         |  ),
         |  Term.Name("+"),
         |  Type.ArgClause(Nil),
         |  Term.ArgClause(List(
         |    Lit.Int(1)
         |  ))
         |)""".stripMargin,
    )
  }

  test("pattern with a pattern unquote") {
    val parsed = Parse.parsePat(Input.String("Some($a)"), dialect.unquotePat(multiline = false))
    assertEquals(
      parsed.get.structure,
      """|Pat.Extract(
         |  Term.Name("Some"),
         |  Pat.ArgClause(List(
         |    Pat.Quasi(
         |      0,
         |      Pat.Var(Term.Name("a"))
         |    )
         |  ))
         |)""".stripMargin,
    )
  }

  test("source with a comment, two-argument apply") {
    val code =
      """|package a
         |
         |object B
         |// c1
         |""".stripMargin
    val tree = Parse.parseSource(Input.String(code), dialect).get
    assertEquals(
      tree.collect { case t: Pkg => t.pos.text },
      List(
        """|package a
           |
           |object B
           |// c1""".stripMargin,
      ),
    )
  }

  test("source with a comment, two-argument apply under global options") {
    val code =
      """|package a
         |
         |object B
         |// c1
         |""".stripMargin
    val options = ParserOptions.default.withCaptureComments(false)
    val tree = ParserOptions.global
      .withValue(options)(Parse.parseSource(Input.String(code), dialect).get)
    assertEquals(
      tree.collect { case t: Pkg => t.pos.text },
      List(
        """|package a
           |
           |object B""".stripMargin,
      ),
    )
  }

}
