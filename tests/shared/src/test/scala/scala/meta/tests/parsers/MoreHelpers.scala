package scala.meta.tests.parsers

import scala.meta._
import scala.meta.internal.parsers._
import scala.meta.tokenizers.TokenizerOptions
import scala.meta.trees.Origin

import munit._

object MoreHelpers {
  def requireNonEmptyOrigin(tree: Tree): tree.type = {
    val missingOrigin = tree.collect { case t if t.origin == Origin.None => t }
    Assertions.assertEquals(
      missingOrigin.map(_.structure),
      Nil,
      "Expected all trees to have non-empty `.origin`.\n" +
        "To fix this failure, update ScalametaParser to use `autoPos()` where the trees below got constructed.\n" +
        "Pro tip: you may also want to add a PositionSuite test for this tree node to verify that the position you set is correct."
    )
    tree
  }
  implicit class XtensionCode(private val code: String) extends AnyVal {
    def asInput(implicit tokenizerOptions: TokenizerOptions): Input = Input.String(code)
      .withTokenizerOptions(tokenizerOptions)
    def asAmmoniteInput(implicit tokenizerOptions: TokenizerOptions): Input = Input.Ammonite(asInput)
    def applyRule[T <: Tree](
        rule: ScalametaParser => T
    )(implicit dialect: Dialect, tokenizerOptions: TokenizerOptions): T = asInput.applyRule(rule)
    def parseRule[T <: Tree](
        rule: ScalametaParser => T
    )(implicit dialect: Dialect, tokenizerOptions: TokenizerOptions): T = asInput.parseRule(rule)
  }
  implicit class XtensionInput(private val input: Input) extends AnyVal {
    def applyRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T =
      requireNonEmptyOrigin(rule(new ScalametaParser(input)))
    def parseRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T =
      applyRule(_.parseRule(rule))
  }
}
