package scala.meta.tests.semanticdb

import munit.FunSuite
import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.metap.Format
import scala.meta.tests.metacp.Library

class PrintSuite extends FunSuite {
  val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath(),
    includeJdk = true
  )

  val compiler = InteractiveSemanticdb.newCompiler()
  val printerSymtab: PrinterSymtab = new PrinterSymtab {
    override def info(symbol: String): Option[SymbolInformation] = symtab.info(symbol)
  }

  def checkDocument(
      name: String,
      original: String,
      expected: String,
      fn: s.TextDocument => Unit
  ): Unit = {
    test(name) {
      val wrapped =
        s"""
object Wrapped {
$original
}
""".stripMargin
      val doc = InteractiveSemanticdb.toTextDocument(
        compiler = compiler,
        code = wrapped,
        options = List(
          "-P:semanticdb:synthetics:on",
          "-P:semanticdb:text:on"
        )
      )
      fn(doc)
    }
  }

  def checkType(symbol: String, expected: String): Unit = {
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
  }

  def checkConstant(constant: s.Constant, expected: String): Unit = {
    test(constant.toString) {
      val obtained = Print.constant(constant)
      assertNoDiff(obtained, expected)
    }
  }

  def checkSignature(symbol: String, expected: String): Unit = {
    test("signature - " + symbol) {
      val info = symtab.info(symbol).get
      val obtained = Print.signature(Format.Compact, info.signature, printerSymtab)
      assertNoDiff(obtained, expected)
    }
  }

  def checkInfo(symbol: String, expected: String): Unit = {
    test("info - " + symbol) {
      val info = symtab.info(symbol).get
      val obtained = Print.info(Format.Compact, info, printerSymtab)
      assertNoDiff(obtained, expected)
    }
  }

  def checkSynthetics(original: String, expected: String): Unit = {
    checkDocument(
      "synthetic - " + original,
      original,
      expected,
      { doc =>
        val obtained = doc.synthetics.map { synthetic =>
          Print.synthetic(Format.Compact, doc, synthetic, printerSymtab)
        }
        assertNoDiff(obtained.mkString("\n"), expected)
      }
    )
  }

  def checkTrees(original: String, expected: String): Unit = {
    checkDocument(
      "trees - " + original,
      original,
      expected,
      { doc =>
        val obtained = doc.synthetics.map { synthetic =>
          Print.tree(Format.Compact, doc, synthetic.tree, printerSymtab)
        }
        assertNoDiff(obtained.mkString("\n"), expected)
      }
    )
  }

  checkType("java/io/ByteArrayOutputStream#buf.", "Array[Byte]")
  checkType("scala/Predef.ArrowAssoc#`->`().", "Tuple2[A, B]")

  checkConstant(s.UnitConstant(), "()")
  checkConstant(s.LongConstant(64), "64L")
  checkConstant(s.CharConstant('a'.toInt), "'a'")
  checkConstant(s.StringConstant("a"), "\"a\"")

  checkSignature(
    "scala/Predef.assert(+1).",
    """(assertion: Boolean, message: => Any): Unit"""
  )
  checkSignature(
    "scala/Predef.ArrowAssoc#`->`().",
    "[B](y: B): Tuple2[A, B]"
  )

  checkInfo(
    "scala/Predef.assert(+1).",
    """scala/Predef.assert(+1). => @inline @elidable final method assert(assertion: Boolean, message: => Any): Unit"""
  )
  checkInfo(
    "scala/Any#",
    """scala/Any# => abstract class Any { +10 decls }"""
  )
  checkInfo(
    "java/util/Collections#singletonList().",
    """java/util/Collections#singletonList(). => static method singletonList[T](param0: T): List[T]"""
  )

  checkSynthetics(
    "List(1).map(_ + 2)",
    """|[2:0..2:18): List(1).map(_ + 2) => *(List.canBuildFrom[Int])
       |[2:0..2:11): List(1).map => *[Int, List[Int]]
       |[2:0..2:4): List => *.apply[Int]
       |""".stripMargin
  )

  checkTrees(
    "List(1).map(_ + 2)",
    """|orig(List(1).map(_ + 2))(List.canBuildFrom[Int])
       |orig(List(1).map)[Int, List[Int]]
       |orig(List).apply[Int]
       |""".stripMargin
  )
}
