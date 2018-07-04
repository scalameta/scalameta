package scala.meta.tests.semanticdb

import org.scalatest.FunSuite
import scala.meta.internal.semanticdb.Scala._

class SymbolSuite extends FunSuite {

  def checkMultiSyntax(symbols: List[String], expected: String): Unit = {
    test(" syntax: " + symbols.toString()) {
      val obtained = Symbols.Multi(symbols)
      assert(obtained == expected)
    }
  }

  def checkMultiRoundtrip(symbols: List[String]): Unit = {
    test("  multi: " + symbols.toString) {
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

  def check(sym: String)(f: String => Boolean): Unit = {
    test(sym) {
      assert(f(sym))
    }
  }

  checkMultiSyntax(Nil, "")
  checkMultiSyntax("a." :: Nil, "a.")
  checkMultiSyntax("a." :: "a." :: Nil, "a.")
  checkMultiSyntax("a." :: "b." :: Nil, ";a.;b.")
  checkMultiSyntax(";a.;b." :: ";c.;d." :: Nil, ";a.;b.;c.;d.")

  checkMultiRoundtrip(Nil)
  checkMultiRoundtrip("com/Bar#" :: Nil)
  checkMultiRoundtrip("com/Bar#" :: "com.Bar." :: Nil)
  checkMultiRoundtrip("com/`; ;`#" :: "com.`; ;`." :: Nil)
  checkMultiRoundtrip("a" :: "b" :: "" :: Nil)
  checkMultiRoundtrip(";_root_/;_empty_/" :: "_star_." :: Nil)

  checkGlobal("com/Bar#")
  checkGlobal("com/Bar.")
  checkGlobal("com/Bar.(a)")
  checkGlobal("com/Bar.[a]")
  checkGlobal(Symbols.RootPackage)
  checkGlobal(Symbols.EmptyPackage)
  checkNotGlobal(";com/Bar#;com/Bar.")
  checkNotGlobal("local1")
  checkNotGlobal(Symbols.None)

  checkLocal("local1")
  checkNotLocal(";local1;local2")
  checkNotLocal("com/Bar#")
  checkNotLocal(";com/Bar#;com/Bar.")
  checkNotLocal(Symbols.None)
  checkNotLocal(Symbols.RootPackage)
  checkNotLocal(Symbols.EmptyPackage)

  check("com/Predef.")(_.isTerm)
  check("com/Class#")(_.isType)
  check("com/")(_.isPackage)
  check(";com/;org/")(!_.isPackage)
  check("com/Class#(a)")(_.isParameter)
  check("com/Class#[A]")(_.isTypeParameter)

}
