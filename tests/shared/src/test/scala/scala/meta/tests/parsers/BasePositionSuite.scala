package scala.meta.tests.parsers

import scala.meta.Dialect
import scala.meta.internal.inputs._
import scala.meta.parsers.Parse

import munit.Location
import munit.TestOptions

abstract class BasePositionSuite(defaultDialect: Dialect) extends ParseSuite {
  import scala.meta._

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
  def checkPositions[T <: Tree: Parse](code: TestOptions, expected: String)(implicit
      loc: Location
  ): Unit = test(code) {
    implicit val D = defaultDialect
    val tree = MoreHelpers.requireNonEmptyOrigin(code.name.parse[T].get)
    val tokens = tree.collect {
      // Reduce the expected output by ignoring lines that can be trivially
      // verified. A line can be trivially verified when you can re-print the
      // `.syntax` without using tokens. For example, if a Mod.Lazy tree has
      // the syntax "lazy" then it's trivially verified and excluded from the
      // output.
      case t if t eq tree => Nil
      case t @ Lit(value) if t.syntax == value.toString => Nil
      case t @ Lit.Unit() if t.syntax == "()" => // This case is needed for Scala.js.
        Nil
      case t @ Name(value) if t.syntax == value => Nil
      case t @ Importee.Name(Name(value)) if t.syntax == value => Nil
      case t @ Pat.Var(Name(value)) if t.syntax == value => Nil
      case t: Mod if s"Mod.${t.syntax.capitalize}" == t.productPrefix => Nil
      case t: Type.Param if t.syntax == t.name.value => Nil
      case t @ Term.Param(Nil, name, Some(tpe), _) if t.syntax == s"$name: $tpe" => Nil
      case t @ Init(Type.Name(value), anon, Nil) if t.syntax == value => Nil
      case t: Importee.Wildcard if t.syntax == "_" => Nil
      case t: Pat.Wildcard if t.syntax == "_" => Nil
      case t @ Term.ArgClause(arg :: Nil, None) if t.syntax == arg.syntax => Nil
      case t @ Pat.ArgClause(arg :: Nil) if t.syntax == arg.syntax => Nil
      case t =>
        val syntax = t.syntax
        val out =
          if (syntax.isEmpty) {
            val (leading, trailing) = t.pos.lineContent.splitAt(t.pos.startColumn)
            s"${t.productPrefix} $leading@@$trailing"
          } else s"${t.productPrefix} $syntax"
        List(out)
    }
    val obtained = tokens.flatten.mkString("\n")
    assertNoDiff(obtained, expected)
  }
}
