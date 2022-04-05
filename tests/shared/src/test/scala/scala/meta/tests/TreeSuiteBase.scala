package scala.meta.tests

import munit._

import scala.meta._

abstract class TreeSuiteBase extends FunSuite {

  protected def assertTree(obtained: Tree)(expected: Tree)(implicit loc: munit.Location): Unit =
    assertNoDiff(obtained.structure, expected.structure)

  protected def assertTree(obtained: Option[Tree])(expected: Option[Tree])(
      implicit loc: munit.Location
  ): Unit =
    (obtained, expected) match {
      case (Some(o), Some(e)) => assertTree(o)(e)
      case _ => assertEquals(obtained, expected)
    }

  protected def assertSyntax(obtained: Tree, syntax: String = null)(expected: Tree)(
      implicit loc: munit.Location
  ): Unit =
    assertNoDiff(obtained.syntax, Option(syntax).getOrElse(expected.syntax), expected.structure)

  protected def checkTree(obtained: Tree, syntax: String = null)(expected: Tree)(
      implicit loc: munit.Location
  ): Unit = {
    assertTree(obtained)(expected)
    assertSyntax(obtained, syntax)(expected)
  }

}
