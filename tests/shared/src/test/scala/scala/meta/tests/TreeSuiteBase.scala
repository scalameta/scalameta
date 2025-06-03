package scala.meta.tests

import org.scalameta.internal.ScalaCompat
import scala.meta._
import scala.meta.internal.inputs._
import scala.meta.tests.parsers.CommonTrees
import scala.meta.tokenizers.Tokenize
import scala.meta.tokenizers.TokenizerOptions
import scala.meta.trees.Origin

import munit._

abstract class TreeSuiteBase extends FunSuite with CommonTrees {

  protected val EOL = org.scalameta.internal.ScalaCompat.EOL
  protected implicit def tokenizerOptions: TokenizerOptions = TokenizerOptions.global.value

  def emptyArgClause = Seq.empty[Term.ArgClause]

  protected def assertStruct(obtained: Tree, extraClue: String = "")(
      expected: String
  )(implicit loc: Location): Unit = {
    def msg = TestHelpers.getMessageWithExtraClue("tree structure not equal", extraClue)
    assertNoDiff(obtained.structure, expected, msg)
  }

  protected def assertTree(obtained: Tree, extraClue: String = "")(expected: Tree)(implicit
      loc: Location
  ): Unit = assertTreeStruct(obtained, extraClue)(expected, expected.structure)

  protected def assertTreeStruct(
      obtained: Tree,
      extraClue: String = ""
  )(expected: Tree, expectedStruct: String)(implicit loc: Location): Unit = {
    assertStruct(obtained, extraClue)(expectedStruct)
    expected.origin match {
      case Origin.None =>
        fail(TestHelpers.getMessageWithExtraClue("origin should not be None", extraClue))
      case _ =>
    }
  }

  protected def assertTrees(obtained: Tree*)(expected: Tree*)(implicit loc: Location): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => assertTree(o)(e) }
  }

  protected def assertTree(
      obtained: Option[Tree]
  )(expected: Option[Tree])(implicit loc: Location): Unit = (obtained, expected) match {
    case (Some(o), Some(e)) => assertTree(o)(e)
    case _ => assertEquals(obtained, expected)
  }

  protected def assertSyntax(
      syntax: String
  )(obtained: Tree)(implicit loc: Location, dialect: Dialect): Unit =
    assertSyntaxWithClue(obtained)(syntax)(obtained.structure)

  protected def assertSyntax(obtained: Tree, syntax: String = null)(
      expected: Tree
  )(implicit loc: Location, dialect: Dialect): Unit =
    assertSyntaxWithClue(obtained, syntax)(expected, expected.structure)

  protected def assertSyntaxWithClue(
      obtained: Tree
  )(syntax: String)(clue: => Any)(implicit loc: Location, dialect: Dialect): String = {
    val reprinted = obtained.reprint
    if (syntax.nonEmpty) assertNoDiff(reprinted, syntax, clue)
    reprinted
  }

  protected def assertSyntaxWithClue(
      obtained: Tree,
      syntax: String = null
  )(expected: Tree, clue: => Any)(implicit loc: Location, dialect: Dialect): Unit =
    assertSyntaxWithClue(obtained)(TestHelpers.getSyntax(expected.reprint, syntax))(clue)

  protected def checkTree(obtained: Tree, syntax: String = null)(
      expected: Tree
  )(implicit loc: Location, dialect: Dialect): Unit = {
    val expectedStruct = expected.structure
    assertTreeStruct(obtained)(expected, expectedStruct)
    assertSyntaxWithClue(obtained, syntax)(expected, expectedStruct)
  }

  protected def checkTrees(
      obtained: Tree*
  )(expected: Tree*)(implicit loc: Location, dialect: Dialect): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => checkTree(o)(e) }
  }

  protected def checkTreesWithSyntax(
      obtained: Tree*
  )(syntax: String*)(expected: Tree*)(implicit loc: Location, dialect: Dialect): Unit = {
    assertEquals(obtained.length, syntax.length)
    checkTreesWithSyntax(obtained.zip(syntax): _*)(expected: _*)
  }

  protected def checkTreesWithSyntax(
      obtained: (Tree, String)*
  )(expected: Tree*)(implicit loc: Location, dialect: Dialect): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case ((o, s), e) => checkTree(o, s)(e) }
  }

  protected implicit class implicitTree[T <: Tree](tree: T) {
    def reprint(implicit dialect: Dialect): String = scala.meta.internal.prettyprinters.TreeSyntax
      .reprint(tree).toString
  }

  protected implicit class ImplicitString(value: String) {
    def tq(repl: String = "QQQ"): String = value.replace(repl, "\"\"\"")
    def lf2nl: String = if (ScalaCompat.EOL == "\n") value else value.replace("\n", ScalaCompat.EOL)
    def nl2lf: String = if (ScalaCompat.EOL == "\n") value else value.replace(ScalaCompat.EOL, "\n")
  }

  protected def assertOriginalSyntax(tree: Tree, original: String)(implicit loc: Location): Unit =
    assertNoDiff(tree.toString, original, tree.structure)

  protected def assertWithOriginalSyntax(tree: Tree, original: String, reprinted: String)(implicit
      loc: Location,
      dialect: Dialect
  ): Unit = {
    val struct = tree.structure
    assertNoDiff(tree.reprint, reprinted, struct)
    assertNoDiff(tree.toString, original, struct)
  }

  protected def assertWithOriginalSyntax(
      trees: Tree*
  )(original: String*)(reprinted: String*)(implicit loc: Location, dialect: Dialect): Unit = {
    assertEquals(trees.length, original.length)
    assertEquals(trees.length, reprinted.length)
    trees.zip(original).zip(reprinted).foreach { case ((t, o), r) =>
      assertWithOriginalSyntax(t, o, r)
    }
  }

  protected def assertTokensAsStructureLines(tokens: Iterable[Token], expected: String)(implicit
      loc: Location
  ): Unit = assertNoDiff(TestHelpers.tokensAsStructureLines(tokens.iterator), expected)

  protected def assertTokenizedAsStructureLines(
      code: String,
      expected: String,
      dialect: Dialect
  )(implicit loc: Location, conv: TestHelpers.Tokenize): Unit = {
    implicit val implicitDialect: Dialect = dialect
    assertTokensAsStructureLines(conv(code), expected)
  }

  protected def assertTokenizedAsStructureLines(code: String, expected: String)(implicit
      loc: Location,
      dialect: Dialect,
      conv: TestHelpers.Tokenize
  ): Unit = assertTokenizedAsStructureLines(code, expected, dialect)

  def assertTokensAsSyntax(tokens: Iterable[Token], expected: String)(implicit
      loc: Location
  ): Unit = assertNoDiff(TestHelpers.tokensAsSyntax(tokens.iterator), expected)

  def assertTokenizedAsSyntax(code: String, expected: String, dialect: Dialect)(implicit
      loc: Location,
      conv: TestHelpers.Tokenize
  ): Unit = {
    implicit val implicitDialect: Dialect = dialect
    assertTokensAsSyntax(conv(code), expected)
  }

  def assertTokenizedAsSyntax(code: String, expected: String)(implicit
      loc: Location,
      dialect: Dialect,
      conv: TestHelpers.Tokenize
  ): Unit = assertTokenizedAsSyntax(code, expected, dialect)

  protected def tokenize(code: String)(implicit dialect: Dialect): Tokens = Tokenize
    .scalametaTokenize.apply(Input.String(code).withTokenizerOptions, dialect).get

  /**
   * Position tests assert that the position of tree nodes enclose the expected source range.
   *
   * Position tests are declared like this
   * {{{
   *   checkPositions[Type](
   *     "[X] =>> (X, X)",
   *     """|Type.Bounds [X@@] =>> (X, X)
   *        |Type.Tuple (X, X)
   *        |""".stripMargin
   *   )
   * }}}
   *
   * Every line in the output format shows the AST tree node type and the tokens for that tree node.
   * Offset positions are rendered as "@@".
   *
   * Below is an example bug that is easy to catch with position tests.
   *
   * {{{
   *   checkPositions[Stat](
   *     "trait A { self: B => }",
   *     """|Ctor.Primary trait A @@{ self: B => }
   *        |Name.Anonymous {
   *        |Template { self: B => }
   *        |Self self: B
   *        |""".stripMargin
   *   )
   * }}}
   *
   * Observe that the line {{{Name.Anonymous {}}} indicates that the position of the anonymous name
   * encloses the `{` token. The correct output should be
   * {{{Name.Anonymous trait A @@{ self: B =>}}}.
   */
  def assertPositions(
      tree: Tree,
      expected: String,
      showPosition: Boolean = false,
      showFieldName: Boolean = false,
      skipFullTree: Boolean = true
  )(implicit loc: Location): Unit = {
    val sb = new StringBuilder
    TestHelpers.collect(tree) {
      // Reduce the expected output by ignoring lines that can be trivially
      // verified. A line can be trivially verified when you can re-print the
      // `.syntax` without using tokens. For example, if a Mod.Lazy tree has
      // the syntax "lazy" then it's trivially verified and excluded from the
      // output.
      case `tree` if skipFullTree =>
      case t: Lit.Null if t.text == "null" =>
      case t: Lit.Unit if t.text == "()" => // This case is needed for Scala.js.
      case t: Lit if t.value != null && t.text == t.value.toString =>
      case t: Name if t.text == t.value =>
      case t @ Importee.Name(Name(value)) if t.text == value =>
      case t @ Pat.Var(Name(value)) if t.text == value =>
      case t: Mod if s"Mod.${t.text.capitalize}" == t.productPrefix =>
      case t: Type.Param if t.text == t.name.value =>
      case t @ Term.Param(Nil, name, Some(tpe), _) if t.text == s"$name: $tpe" =>
      case t @ Init(Type.Name(value), anon, Nil) if t.text == value =>
      case t: Importee.Wildcard if t.text == "_" =>
      case t: Pat.Wildcard if t.text == "_" =>
      case t @ Term.ArgClause(arg :: Nil, None) if t.text == arg.text =>
      case t @ Pat.ArgClause(arg :: Nil) if t.text == arg.text =>
      case t =>
        object IterableIndex {
          def unapply(obj: Any): Option[Int] = obj match {
            case x: Iterable[_] => x.zipWithIndex.collectFirst { case (`t`, idx) => idx }
            case _ => None
          }
        }
        val nameOpt =
          if (showFieldName) t.parent.flatMap(p =>
            p.productFields.iterator.zip(p.productIterator).collectFirst {
              case (name, `t`) => name
              case (name, Some(`t`)) => name
              case (name, IterableIndex(idx)) => s"$name$idx"
            }
          ).orElse(Some("?"))
          else None
        nameOpt.foreach(x => sb.append('<').append(x).append('>'))
        sb.append(t.productPrefix).append(' ')
        val pos = t.pos
        val syntax = t.text
        if (syntax.nonEmpty) sb.append(syntax)
        else if (pos eq Position.None) sb.append("@?@")
        else {
          val (leading, trailing) = pos.lineContent.splitAt(pos.startColumn)
          sb.append(leading).append("@@").append(trailing)
        }
        nameOpt.foreach(x => sb.append("</").append(x).append('>'))
        if (showPosition) sb.append(' ').append(pos.desc)
        sb.append('\n')
    }
    val obtained = sb.result()
    assertNoDiff(obtained, expected)
  }

}
