package scala.meta.tests

import munit._

import scala.meta._
import meta.prettyprinters.XtensionStructure

// Since currently the UnApply signatures are not being typed (instead always producing `Any`)
// in the scala 3 version of quasiquotes, we naively cast to the correct type here, to keep
// the test suites themselves simple 
abstract class TreeSuiteBase extends FunSuite {

  protected def assertTree(obtained: Any)(expected: Tree)(implicit loc: munit.Location): Unit =
    assertNoDiff(obtained.asInstanceOf[Tree].structure, expected.structure)

  protected def assertTree(obtained: Tree)(expected: Tree)(implicit loc: munit.Location): Unit =
    assertNoDiff(obtained.structure, expected.structure)

  protected def assertTrees(
      obtained: Any
  )(expected: Tree*)(implicit loc: munit.Location): Unit = assertTrees(obtained.asInstanceOf[Seq[Tree]]: _*)(expected: _*)

  protected def assertTrees(
      obtained: Tree*
  )(expected: Tree*)(implicit loc: munit.Location): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => assertTree(o)(e) }
  }

  protected def assertTree(obtained: Any)(expected: Option[Tree])(
      implicit loc: munit.Location
  ): Unit =
    (obtained.asInstanceOf[Option[Tree]], expected) match {
      case (Some(o), Some(e)) => assertTree(o)(e)
      case _ => assertEquals(obtained, expected)
    }

  protected def assertTree(obtained: Option[Tree])(expected: Option[Tree])(
      implicit loc: munit.Location
  ): Unit =
    (obtained, expected) match {
      case (Some(o), Some(e)) => assertTree(o)(e)
      case _ => assertEquals(obtained, expected)
    }

  protected def assertSyntax(obtained: Any, syntax: String = null)(expected: Tree)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit =
    assertNoDiff(obtained.asInstanceOf[Tree].reprint, Option(syntax).getOrElse(expected.reprint), expected.structure)

  protected def checkTree(obtained: Any, syntax: String = null)(expected: Tree)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertTree(obtained)(expected)
    assertSyntax(obtained, syntax)(expected)
  }

  protected def checkTrees(obtained: Tree*)(expected: Tree*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case (o, e) => checkTree(o)(e) }
  }

  protected def checkTreesWithSyntax(obtained: Any*)(syntax: String*)(expected: Tree*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(obtained.length, syntax.length)
    checkTreesWithSyntax(obtained.zip(syntax): _*)(expected: _*)
  }

  protected def checkTreesWithSyntax(obtained: (Any, String)*)(expected: Tree*)(
      implicit loc: munit.Location,
      dialect: Dialect
  ): Unit = {
    assertEquals(obtained.length, expected.length)
    obtained.zip(expected).foreach { case ((o, s), e) => checkTree(o, s)(e) }
  }

  protected implicit class implicitTree[T <: Tree](tree: T) {
    def reprint(implicit dialect: Dialect): String =
      scala.meta.internal.prettyprinters.TreeSyntax.reprint(tree).toString
  }

}
