package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb._

import munit.FunSuite

abstract class JavacpSuiteBase extends FunSuite {
  private val infos = MetacMetacpDiffExpect.metacpSymbols

  def checkOrder(name: String, symbol: String, filter: String => Boolean, expected: List[String])(
      implicit loc: munit.Location,
  ): Unit = test(name) {
    val info = infos(symbol)
    val ClassSignature(_, _, _, Some(declarations)) = info.signature
    val obtained = declarations.symlinks.filter(filter)
    assertEquals(obtained, expected, info.toProtoString)
  }

  def checkSynthetic(name: String, symbol: String, expected: Boolean)(implicit
      loc: munit.Location,
  ): Unit = test(name) {
    val info = infos(symbol)
    assert(info.isSynthetic == expected, info.toProtoString)
  }

  // #1492: the JLS-mandated enum methods `values`/`valueOf` are compiler-synthesized, while
  // user-written members and an ordinary parameter of `valueOf` are not. (Fixture: Coin.)
  checkSynthetic("enum values() is synthetic", "com/javacp/Coin#values().", expected = true)
  checkSynthetic("enum valueOf() is synthetic", "com/javacp/Coin#valueOf().", expected = true)
  checkSynthetic("user enum method not synthetic", "com/javacp/Coin#value().", expected = false)
  checkSynthetic("user enum field not synthetic", "com/javacp/Coin#value.", expected = false)
  checkSynthetic(
    "valueOf name param not synthetic",
    "com/javacp/Coin#valueOf().(name)",
    expected = false,
  )

  // #1492: detection matches the JLS-mandated *signature*, not just the name — a same-named user
  // overload with a different signature must NOT be synthetic. (Fixture: EnumOverloads, which adds
  // `valueOf(int)` and `values(String)` overloads alongside the generated `valueOf(String)`/`values()`.)
  checkSynthetic("enum valueOf(String) is synthetic", "com/javacp/EnumOverloads#valueOf().", true)
  checkSynthetic("enum values() is synthetic", "com/javacp/EnumOverloads#values().", true)
  checkSynthetic(
    "valueOf(int) overload not synthetic",
    "com/javacp/EnumOverloads#valueOf(+1).",
    false,
  )
  checkSynthetic(
    "values(String) overload not synthetic",
    "com/javacp/EnumOverloads#values(+1).",
    false,
  )

}
