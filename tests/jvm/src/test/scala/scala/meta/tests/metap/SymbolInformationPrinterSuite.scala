package scala.meta.tests.metap
import org.scalatest.FunSuite
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.metap.SymbolInformationPrinter
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.testkit.DiffAssertions
import scala.meta.tests.metacp.Library

class SymbolInformationPrinterSuite extends FunSuite with DiffAssertions {
  val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++
      Library.jdk.classpath()
  )
  val printerSymtab: PrinterSymtab = new PrinterSymtab {
    override def info(symbol: String): Option[SymbolInformation] = symtab.info(symbol)
  }

  def check(symbol: String, expected: String): Unit = {
    test(symbol) {
      val info = symtab.info(symbol).get
      val obtained = SymbolInformationPrinter.print(info, printerSymtab)
      assertNoDiffOrPrintExpected(obtained, expected)
    }
  }

  check(
    "scala/Predef.assert().",
    """scala/Predef.assert(). => @elidable method assert(assertion: Boolean): Unit"""
  )

  check(
    "scala/Any#",
    """scala/Any# => abstract class Any { +10 decls }"""
  )

  check(
    "java/util/Collections#singletonList().",
    """java/util/Collections#singletonList(). => static method singletonList[T](param0: T): List[T]"""
  )

}
