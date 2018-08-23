package scala.meta.tests.semanticdb

import org.scalatest.FunSuite
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.metap.SymbolInformationPrinter
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.metap.Format
import scala.meta.testkit.DiffAssertions
import scala.meta.tests.metacp.Library

class PrintSuite extends FunSuite with DiffAssertions {
  val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++
      Library.jdk.classpath()
  )
  val printerSymtab: PrinterSymtab = new PrinterSymtab {
    override def info(symbol: String): Option[SymbolInformation] = symtab.info(symbol)
  }

  def checkInfo(symbol: String, expected: String): Unit = {
    test(symbol) {
      val info = symtab.info(symbol).get
      val obtained = Print.info(Format.Compact, info, printerSymtab)
      assertNoDiffOrPrintExpected(obtained, expected)
    }
  }

  checkInfo(
    "scala/Predef.assert().",
    """scala/Predef.assert(). => @elidable method assert(assertion: Boolean): Unit"""
  )

  checkInfo(
    "scala/Any#",
    """scala/Any# => abstract class Any { +10 decls }"""
  )

  checkInfo(
    "java/util/Collections#singletonList().",
    """java/util/Collections#singletonList(). => static method singletonList[T](param0: T): List[T]"""
  )

}
