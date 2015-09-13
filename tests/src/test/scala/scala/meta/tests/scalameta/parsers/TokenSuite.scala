package scala.meta.tests
package scalameta
package parsers

import scala.meta.dialects.Scala211

class TokenSuite extends ParseSuite {
  test("class C") {
    templStat("@foo\n//bar bar\ndef baz = qux")
  }
}
