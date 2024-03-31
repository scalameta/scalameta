package scala.meta.tests.testkit
package prettyprinters

import org.scalameta.logger
import scala.meta._
import scala.meta.testkit.StructurallyEqual
import scala.meta.trees.Origin

import munit.FunSuite

class PrettyPrinterSuite extends FunSuite {
  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    def resetAllOrigins: T = tree.withOriginRecursive(Origin.None)
  }

  def checkOk(code: String): Unit = test(logger.revealWhitespace(code)) {

    val before: Stat = code.parse[Stat].get.resetAllOrigins
    val after: Stat = before.syntax.parse[Stat].get

    StructurallyEqual(before, after) match {
      case Left(err) => fail(
          s"""|Not Structurally equal: ${err.toString}:
              |before: ${before.structure}
              |after : ${after.structure}
              |""".stripMargin
        )
      case _ => Nil
    }

  }

  checkOk("val x = 1")

  checkOk("""(_: Throwable) ⇒ 1""")

  checkOk("""1 join (())""")

  checkOk("""foo match{ case _ => _ => false}""")

  checkOk("class Foo(`this`: Int)")

  checkOk("class Foo(foo: String) { def this(foo: Int) = this(foo.toString) }")
}
