package scala.meta.tests
package parsers

import org.scalameta.logger
import scala.meta._
import scala.meta.internal.parsers._

import scala.language.implicitConversions

class ParseSuite extends TreeSuiteBase with CommonTrees {
  import MoreHelpers._

  val EOL = org.scalameta.internal.ScalaCompat.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""

  implicit def parseStat(code: String)(implicit dialect: Dialect): Stat = templStat(code)
  implicit def parseSource(code: String)(implicit dialect: Dialect): Source = source(code)
  implicit def parseType(code: String)(implicit dialect: Dialect): Type = tpe(code)
  implicit def parsePat(code: String)(implicit dialect: Dialect): Pat = pat(code)
  implicit def parseCaseTree(code: String)(implicit dialect: Dialect): Case = parseCase(code)

  // This should eventually be replaced by DiffAssertions.assertNoDiff
  def assertSameLines(actual: String, expected: String)(implicit loc: munit.Location) = {
    val actualLines = actual.linesIterator.toList
    val expectedLines = expected.linesIterator.toList
    assertEquals(actualLines, expectedLines)
  }

  def stat(code: String)(implicit dialect: Dialect) = code.applyRule(_.parseStat())

  def parseRule[T <: Tree](code: String, f: ScalametaParser => T)(implicit dialect: Dialect) = code
    .parseRule(f)
  def term(code: String)(implicit dialect: Dialect) = parseRule(code, _.expr())
  def pat(code: String)(implicit dialect: Dialect) = parseRule(code, _.pattern())
  def patternTyp(code: String)(implicit dialect: Dialect) = parseRule(code, _.patternTyp())
  def tpe(code: String)(implicit dialect: Dialect) = parseRule(code, _.typ())
  def topStat(code: String)(implicit dialect: Dialect) =
    parseRule(code, p => p.statSeq(p.topStat).head)
  def templStat(code: String)(implicit dialect: Dialect) =
    parseRule(code, p => p.statSeq(p.templateStat()).head)
  def blockStat(code: String)(implicit dialect: Dialect) = parseRule(code, _.blockStatSeq().head)
  def parseCase(code: String)(implicit dialect: Dialect) = code.applyRule(_.parseCase())
  def source(code: String)(implicit dialect: Dialect) = parseRule(code, _.source())

  def ammonite(code: String)(implicit dialect: Dialect) = code.asAmmoniteInput
    .parseRule(_.entryPointAmmonite())

  def testParseErrors(stats: String*)(implicit loc: munit.Location, dialect: Dialect) = stats
    .foreach(x => test(x)(interceptParseError(x)))

  def interceptParseError(stat: String)(implicit loc: munit.Location, dialect: Dialect): String =
    try intercept[parsers.ParseException](templStat(stat)).getMessage().nl2lf
    catch {
      case scala.util.control.NonFatal(t) =>
        val msg = "no exception was thrown"
        val richFeedback = t.getMessage.replace(msg, s"$msg for '$stat'")
        fail(richFeedback)
    }

  def checkError(stat: String)(implicit dialect: Dialect) =
    test(logger.revealWhitespace(stat).take(50))(interceptParseError(stat))
  def checkOK(stat: String)(implicit dialect: Dialect) =
    test(logger.revealWhitespace(stat).take(50))(templStat(stat))

  protected def checkParsedTree[T <: Tree](
      code: String,
      f: ScalametaParser => T,
      syntax: String = null
  )(tree: Tree)(implicit loc: munit.Location, dialect: Dialect): Unit =
    checkTree(parseRule(code, f), TestHelpers.getSyntax(code, syntax))(tree)

  protected def checkStat(code: String, syntax: String = null)(
      tree: Tree
  )(implicit loc: munit.Location, dialect: Dialect): Unit =
    checkParsedTree(code, _.entrypointStat(), syntax)(tree)

  protected def runTestError[T <: Tree](code: String, expected: String)(implicit
      parser: String => T,
      loc: munit.Location
  ): Unit = {
    val error = intercept[inputs.InputException] {
      val result = parser(code)
      throw new ParseException(
        Position.None,
        s"Statement $code should not parse! Got result ${result.structure}"
      )
    }
    val obtained = error.getMessage().nl2lf
    assert(obtained.contains(expected), s"Expected [$obtained] to contain [$expected].")
  }

  protected def matchSubStructure[T <: Tree](
      code: String,
      expected: PartialFunction[Tree, Unit]
  )(implicit parser: String => T, loc: munit.Location): Unit = {
    val obtained = parser(code)
    expected.lift(obtained).getOrElse(fail(s"Got unexpected tree: ${obtained.structure}"))
  }

  /**
   * Check if code can be parsed to expected syntax tree.
   *
   * @see
   *   runTestAssert(code, assertLayout)(expected)
   */
  protected def runTestAssert[T <: Tree](code: String, assertLayout: Option[String])(
      expected: Tree
  )(implicit loc: munit.Location, parser: String => T, dialect: Dialect): Unit =
    runTestAssert[T](code, assertLayout.getOrElse(""))(expected)

  /**
   * General method used to assert a given 'code' parses to expected tree structure and back. We
   * cannot assert trees by equality(==) that's why we check if they are identical by asserting
   * their structure representation and optionally syntax. If expectedLayout is provided then we
   * print back generated tree structure and assert generated text is equal to expectedLayout (in
   * most cases it should be the same as 'code' param but sometimes formatting is a little different
   * or for safety () are added). If you are not interested in asserting layout just provide None.
   * After printing generated tree to text representation we parse it again. This ensures that
   * invariant holds: parse(code) = parse(print(parse(code))) Reprint cannot be handled by
   * `tree.syntax` because syntax is cached by default and would not be reprinted but only input
   * code would be returned.
   *
   * @param code
   *   valid scala code
   * @param assertLayout
   *   string representation of code to be printed
   * @param expected
   *   provided 'code' should parse to this tree structure
   * @param parser
   *   Function used to convert code into structured tree
   */
  protected def runTestAssert[T <: Tree](code: String, assertLayout: String = null)(
      expected: Tree
  )(implicit loc: munit.Location, parser: String => T, dialect: Dialect): Unit = {
    val struct = expected.structure
    parseAndCheckTreeWithSyntaxAndStruct[T](code, assertLayout, struct, "Original").foreach {
      parseAndCheckTreeWithSyntaxAndStruct[T](_, assertLayout, struct, "Reprinted")
    }
  }

  protected def parseAndCheckTree[T <: Tree](code: String, syntax: String = null)(
      expected: Tree
  )(implicit loc: munit.Location, parser: String => T, dialect: Dialect): Unit =
    parseAndCheckTreeWithSyntaxAndStruct[T](code, syntax, expected.structure)

  private def parseAndCheckTreeWithSyntaxAndStructImpl[T <: Tree](
      code: String,
      syntax: String,
      struct: String,
      extraClue: String = ""
  )(implicit loc: munit.Location, parser: String => T, dialect: Dialect): String = {
    val obtained: T = parser(code)
    val reprinted = assertSyntaxWithClue(obtained)(syntax)(
      TestHelpers.getMessageWithExtraClue("tree syntax not equal", extraClue) + "\n" + struct
    )
    assertStruct(obtained, extraClue)(struct)
    reprinted
  }

  protected def parseAndCheckTreeWithSyntaxAndStruct[T <: Tree](
      code: String,
      syntax: String,
      struct: String,
      extraClue: String = ""
  )(implicit loc: munit.Location, parser: String => T, dialect: Dialect): Option[String] = {
    val codeLF = code.replaceAll("\\r+\\n", "\n")
    val expectedSyntax = TestHelpers.getSyntax(codeLF, syntax)
    val reprinted =
      parseAndCheckTreeWithSyntaxAndStructImpl[T](code, expectedSyntax, struct, extraClue)
    def checkAlternative(altCode: String, altMsg: String): Unit = if (code != altCode) {
      val res =
        parseAndCheckTreeWithSyntaxAndStructImpl[T](altCode, expectedSyntax, struct, extraClue)
      if (expectedSyntax.nonEmpty)
        assertNoDiff(res, reprinted, TestHelpers.getMessageWithExtraClue(altMsg, extraClue))
    }
    checkAlternative(codeLF, "LF vs original")
    checkAlternative(codeLF.replace("\n", "\r\n"), "CRLF vs original")
    checkAlternative(codeLF.replace("\n", "\r"), "CR vs original")
    if (reprinted == code) None else Some(reprinted)
  }

  protected def checkWithOriginalSyntax[T <: Tree](tree: T, originalOpt: String = null)(
      reprinted: String,
      reprintedFails: String = null
  )(implicit loc: munit.Location, parser: String => T, dialect: Dialect): Unit = {
    val original = Option(originalOpt).getOrElse(reprinted)
    assertOriginalSyntax(tree, original)
    if (reprintedFails ne null) {
      runTestError[T](reprinted, reprintedFails)
      if (original != reprinted) checkTree(parser(original), reprinted)(tree)
    } else runTestAssert[T](original, reprinted)(tree)
  }

  protected def scannerTokens(code: String)(implicit dialect: Dialect): Iterable[Token] = {
    val st = ScannerTokens(code.asInput)
    class MyIter extends Iterator[Token] {
      private val ti = LazyTokenIterator(st)
      override def hasNext: Boolean = ti.hasCurr
      override def next(): Token =
        try ti.currToken
        finally ti.next()
    }
    new Iterable[Token] {
      override def iterator: Iterator[Token] = new MyIter
    }
  }

  implicit val implicitTokenize: TestHelpers.Tokenize = new TestHelpers.Tokenize {
    override def apply(code: String)(implicit dialect: Dialect): Iterable[Token] =
      scannerTokens(code)
  }

}
