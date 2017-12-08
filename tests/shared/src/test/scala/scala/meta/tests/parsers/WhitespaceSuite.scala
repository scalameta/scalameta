package scala.meta.tests
package parsers

import scala.meta.dialects.Scala211

class WhitespaceSuite extends ParseSuite {
  test("annot > \n > comment > \n > defn") {
    templStat("@foo\n//bar bar\ndef baz = qux")
  }

  test("annot > whitespace > \n > defn") {
    templStat("@foo \ndef baz = qux")
  }
}
