package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.ClassSignature

import munit.FunSuite

abstract class JavacpSuiteBase extends FunSuite {
  private val infos = MetacMetacpDiffExpect.metacpSymbols

  def checkOrder(name: String, symbol: String, filter: String => Boolean, expected: List[String])(
      implicit loc: munit.Location
  ): Unit = test(name) {
    val info = infos(symbol)
    val ClassSignature(_, _, _, Some(declarations)) = info.signature
    val obtained = declarations.symlinks.filter(filter)
    assertEquals(obtained, expected, info.toProtoString)
  }

}
