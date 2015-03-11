package scala.meta.interpreter.internal.test

sealed trait TestTraitNonFinal
case class XNonFinal() extends TestTraitNonFinal
case class YNonFinal() extends TestTraitNonFinal

sealed trait TestTraitNonCase
final class XNonCase() extends TestTraitNonCase
final class YNonCase() extends TestTraitNonCase

sealed trait TestTrait
final case class X() extends TestTrait
final case class Y() extends TestTrait

case object SerObject
final case class TestCaseClass()
