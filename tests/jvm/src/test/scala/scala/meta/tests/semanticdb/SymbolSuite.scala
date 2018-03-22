package scala.meta.tests.semanticdb

import scala.meta.Symbol
import scala.meta.Signature
import org.scalatest.FunSuite

class SymbolSuite extends FunSuite {
  def check(original: String, expected: Symbol): Unit = {
    test(original) {
      val obtained = Symbol(original)
      assert(obtained == expected)
    }
  }
  val root = Symbol.Global(Symbol.None, Signature.Term("_root_"))
  val a = Symbol.Global(root, Signature.Term("a"))

  check("_root_.", root)
  check("_root_.a.", a)
  check("a.", a)
  check("_root_._root_.a.", Symbol.Global(Symbol.Global(root, Signature.Term("_root_")), Signature.Term("a")))

}
