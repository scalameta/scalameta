package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb._

import munit.FunSuite

class SymbolInformationOpsSuite extends FunSuite {

  private def check(
      prop: SymbolInformation.Property,
  )(is: SymbolInformation => Boolean)(implicit loc: munit.Location): Unit = test(prop.toString) {
    assert(is(SymbolInformation(properties = prop.value)))
    assert(!is(SymbolInformation()))
  }

  check(SymbolInformation.Property.NATIVE)(_.isNative)
  check(SymbolInformation.Property.SYNCHRONIZED)(_.isSynchronized)
  check(SymbolInformation.Property.TRANSIENT)(_.isTransient)
  check(SymbolInformation.Property.VOLATILE)(_.isVolatile)

}
