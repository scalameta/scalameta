package scala.meta.tests.semanticdb

import org.scalatest.FunSuite
import scala.meta.internal.semanticdb3.Scala._

class SymbolSuite extends FunSuite {

  def checkMultiRoundtrip(symbols: List[String]): Unit = {
    test("  multi: " + Symbols.Multi(symbols)) {
      val symbol = Symbols.Multi(symbols)
      val expected = symbol.asMulti
      assert(symbol.asMulti == expected)
    }
  }

  def checkGlobal(symbol: String): Unit = {
    test(" global: " + symbol) { assert(symbol.isGlobal) }
  }

  def checkNotGlobal(symbol: String): Unit = {
    test("!global: " + symbol) { assert(!symbol.isGlobal) }
  }

  def checkLocal(symbol: String): Unit = {
    test("  local: " + symbol) { assert(symbol.isLocal) }
  }

  def checkNotLocal(symbol: String): Unit = {
    test(" !local: " + symbol) { assert(!symbol.isLocal) }
  }

  checkMultiRoundtrip("com.Bar#" :: Nil)
  checkMultiRoundtrip("com.Bar#" :: "com.Bar." :: Nil)
  checkMultiRoundtrip("com.`; ;`#" :: "com.`; ;`." :: Nil)
  checkMultiRoundtrip("a" :: "b" :: "" :: Nil)

  checkGlobal("com.Bar#")
  checkGlobal(";com.Bar#;com.Bar.")
  checkGlobal(Symbols.RootPackage)
  checkGlobal(Symbols.EmptyPackage)
  checkNotGlobal("local1")
  checkNotGlobal(Symbols.None)

  checkLocal("local1")
  checkLocal(";local1;local2")
  checkNotLocal("com.Bar#")
  checkNotLocal(";com.Bar#;com.Bar.")
  checkNotLocal(Symbols.None)
  checkNotLocal(Symbols.RootPackage)
  checkNotLocal(Symbols.EmptyPackage)

}
