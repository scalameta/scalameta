package scala.meta.tests
package parsers

import scala.meta._

class WhitespaceSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  test("annot > \n > comment > \n > defn")(templStat("@foo\n//bar bar\ndef baz = qux"))

  test("annot > whitespace > \n > defn")(templStat("@foo \ndef baz = qux"))
}
