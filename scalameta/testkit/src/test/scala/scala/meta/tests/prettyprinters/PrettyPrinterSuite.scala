package scala.meta
package tests.prettyprinters

import scala.meta.internal.ast.Origin
import scala.meta.testkit.StructurallyEqual

import org.scalameta.logger
import org.scalatest.FunSuite

class PrettyPrinterSuite extends FunSuite {
  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    def resetAllOrigins: T = {
      tree
        .transform {
          case tree: Tree => tree.withOrigin(Origin.None)
        }
        .asInstanceOf[T]
    }
  }
  def checkOk(code: String): Unit = {
    test(logger.revealWhitespace(code)) {
      val before = code.parse[Stat].get.resetAllOrigins
      val prettyPrinted = before.syntax.parse[Stat].get
      StructurallyEqual(before, prettyPrinted) match {
        case Left(err) =>
          fail(err.toString)
        case _ => Nil
      }

    }
  }

  checkOk("val x = 1")
  checkOk(
    """
      |{val directives = sort(flat)
      |
      |      { case x ⇒ directives collectFirst { case (c, d) if c isInstance x ⇒ d } getOrElse Escalate }}
    """.stripMargin)

}
