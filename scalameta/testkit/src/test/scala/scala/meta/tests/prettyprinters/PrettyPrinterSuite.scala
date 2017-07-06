package scala.meta
package tests.prettyprinters

import scala.meta.internal.trees.Origin
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

      val before: Stat = code.parse[Stat].get.resetAllOrigins
      val after: Stat  = before.syntax.parse[Stat].get

      StructurallyEqual(before, after) match {
        case Left(err) =>
          fail(
            s"""Not Structurally equal: ${err.toString}:
               |before: ${before.structure}
               |after : ${after.structure}
             """.stripMargin)
        case _ => Nil
      }

    }
  }

  checkOk("val x = 1")

  checkOk("""(_: Throwable) ⇒ 1""")

  checkOk("""1 join (())""")

  checkOk("""foo match{ case _ => _ => false}""")
}
