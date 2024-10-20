package scala.meta.tests.parsers

import scala.meta.Dialect
import scala.meta.internal.inputs._
import scala.meta.parsers.Parse

import munit.Location
import munit.TestOptions

abstract class BasePositionSuite(defaultDialect: Dialect) extends ParseSuite {
  import scala.meta._
  import scala.meta.tests.parsers.MoreHelpers._

  def checkPositions[T <: Tree: Parse](code: TestOptions)(implicit loc: Location): Unit =
    checkPositions[T](code, "")

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
  def checkPositions[T <: Tree: Parse](
      code: TestOptions,
      expected: String,
      expectedTokens: String = "",
      showPosition: Boolean = false,
      showFieldName: Boolean = false
  )(implicit loc: Location): Unit = test(code) {
    implicit val D = defaultDialect
    if (expectedTokens.nonEmpty) assertTokenizedAsStructureLines(code.name, expectedTokens)
    val tree = code.name.asInput.parse[T]
      .fold(x => fail("parse failure", x.details), MoreHelpers.requireNonEmptyOrigin(_))
    val sb = new StringBuilder
    tree.traverse {
      // Reduce the expected output by ignoring lines that can be trivially
      // verified. A line can be trivially verified when you can re-print the
      // `.syntax` without using tokens. For example, if a Mod.Lazy tree has
      // the syntax "lazy" then it's trivially verified and excluded from the
      // output.
      case `tree` =>
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
          if (showFieldName) t.parent.flatMap { p =>
            p.productFields.iterator.zip(p.productIterator).collectFirst {
              case (name, `t`) => name
              case (name, Some(`t`)) => name
              case (name, IterableIndex(idx)) => s"$name$idx"
            }
          }.orElse(Some("?"))
          else None
        nameOpt.foreach(x => sb.append('<').append(x).append('>'))
        sb.append(t.productPrefix).append(' ')
        val syntax = t.text
        if (syntax.isEmpty) {
          val (leading, trailing) = t.pos.lineContent.splitAt(t.pos.startColumn)
          sb.append(leading).append("@@").append(trailing)
        } else sb.append(syntax)
        nameOpt.foreach(x => sb.append("</").append(x).append('>'))
        if (showPosition) sb.append(' ').append(t.pos.desc)
        sb.append('\n')
    }
    val obtained = sb.result()
    assertNoDiff(obtained, expected)
  }
}
