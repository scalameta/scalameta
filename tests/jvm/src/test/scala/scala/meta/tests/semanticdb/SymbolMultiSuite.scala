package scala.meta.tests.semanticdb

import org.scalatest.FunSuite
import scala.meta.internal.semanticdb3.Scala._

class SymbolMultiSuite extends FunSuite {

  def checkRoundtrip(symbols: List[String]): Unit = {
    test(Symbols.Multi(symbols)) {
      val symbol = Symbols.Multi(symbols)
      val expected = symbol.flattenMulti
      assert(symbol.flattenMulti == expected)

    }
  }

  checkRoundtrip("com.Bar#" :: Nil)
  checkRoundtrip("com.Bar#" :: "com.Bar." :: Nil)
  checkRoundtrip("com.`; ;`#" :: "com.`; ;`." :: Nil)
  checkRoundtrip("a" :: "b" :: "" :: Nil)
}
