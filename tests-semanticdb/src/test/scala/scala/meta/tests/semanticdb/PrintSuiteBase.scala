package scala.meta.tests.semanticdb

import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.metap.Format
import scala.meta.tests.metacp.Library

import munit.FunSuite

abstract class PrintSuiteBase extends FunSuite {
  val symtab = GlobalSymbolTable(Library.scalaLibrary.classpath(), includeJdk = true)

  val compiler = InteractiveSemanticdb.newCompiler()
  val printerSymtab: PrinterSymtab = new PrinterSymtab {
    override def info(symbol: String): Option[SymbolInformation] = symtab.info(symbol)
  }

  def checkDocument(
      name: String,
      original: String,
      expected: String,
      fn: s.TextDocument => Unit
  ): Unit = test(name) {
    val wrapped = s"""
object Wrapped {
$original
}
""".stripMargin
    val doc = InteractiveSemanticdb.toTextDocument(
      compiler = compiler,
      code = wrapped,
      options = List("-P:semanticdb:synthetics:on", "-P:semanticdb:text:on")
    )
    fn(doc)
  }

  def checkType(symbol: String, expected: String)(implicit loc: munit.Location): Unit =
    test("type - " + symbol) {
      val info = symtab.info(symbol).get
      val tpe = info.signature match {
        case s.ValueSignature(tpe) => tpe
        case s.MethodSignature(_, _, tpe) => tpe
        case e => throw new MatchError(e)
      }
      val obtained = Print.tpe(Format.Compact, tpe, printerSymtab)
      assertNoDiff(obtained, expected)
    }

  def checkConstant(constant: s.Constant, expected: String)(implicit loc: munit.Location): Unit =
    test(constant.toString) {
      val obtained = Print.constant(constant)
      assertNoDiff(obtained, expected)
    }

  def checkSignature(symbol: String, expected: String)(implicit loc: munit.Location): Unit =
    test("signature - " + symbol) {
      val info = symtab.info(symbol).get
      val obtained = Print.signature(Format.Compact, info.signature, printerSymtab)
      assertNoDiff(obtained, expected)
    }

  def checkInfo(symbol: String, expected: String)(implicit loc: munit.Location): Unit =
    test("info - " + symbol) {
      val info = symtab.info(symbol).get
      val obtained = Print.info(Format.Compact, info, printerSymtab)
      assertNoDiff(obtained, expected)
    }

  def checkSynthetics(original: String, expected: String)(implicit loc: munit.Location): Unit =
    checkDocument(
      "synthetic - " + original,
      original,
      expected,
      { doc =>
        val obtained = doc.synthetics
          .map(synthetic => Print.synthetic(Format.Compact, doc, synthetic, printerSymtab))
        assertNoDiff(obtained.mkString("\n"), expected)
      }
    )

  def checkTrees(original: String, expected: String)(implicit loc: munit.Location): Unit =
    checkDocument(
      "trees - " + original,
      original,
      expected,
      { doc =>
        val obtained = doc.synthetics
          .map(synthetic => Print.tree(Format.Compact, doc, synthetic.tree, printerSymtab))
        assertNoDiff(obtained.mkString("\n"), expected)
      }
    )

}
