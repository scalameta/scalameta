package scala.meta.tests
package parsers

import scala.meta._

class Scala210Suite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala210

  test("$_") {
    val error = """|<input>:1: error: Not one of: `$$', `$'ident, `$'this, `$'BlockExpr
                   | val q"x + $_" = tree 
                   |            ^""".stripMargin.lf2nl
    interceptMessage[ParseException](error)(stat(""" val q"x + $_" = tree """))
  }

  test("case classes without a parameter list are allowed")(templStat("case class A"))
}
