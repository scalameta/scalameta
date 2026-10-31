package scala.meta.tests.metacp

import scala.meta.internal.metacp._
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.scalacp.SymbolOps._
import scala.meta.internal.scalacp.TypeOps._
import scala.meta.internal.semanticdb.Print
import scala.meta.metap.Format
import scala.meta.tests.semanticdb.MetacpSnippets

import scala.tools.scalap.scalax.rules.scalasig._

import munit.FunSuite

class ScalacpSuite extends FunSuite with MetacpSnippets {

  private lazy val classes = compile {
    """|package a
       |class C(val v: Int, var w: Int, x: Int) {
       |  def this(y: Int) = this(y, y, y)
       |  def f[T](t: T): List[T] = Nil
       |  def g: Int = 1
       |  def h(i: Int)(j: Int): String = ""
       |  val o: Option[Int] = None
       |  def s: o.type = o
       |}
       |""".stripMargin
  }

  // the methods of `a.C`, in the order of its ScalaSig
  private def methods(show: SymbolInfoSymbol => String): String = classes.resolve("a/C.class")
    .toClassNode.scalaSig.get.scalaSig.symbols.collect {
      case m: MethodSymbol if m.isMethod && m.parent.exists(_.path == "a.C") =>
        s"${m.name}: ${show(m)}"
    }.mkString("\n")

  test("ret") {
    val obtained = methods(_.infoType.ret match {
      case t: TypeRefType => t.symbol.path
      case t => t.getClass.getSimpleName
    })
    assertNoDiff(
      obtained,
      """|v: scala.Int
         |w: scala.Int
         |w_$eq: scala.Unit
         |<init>: a.C
         |<init>: a.C
         |f: scala.package.List
         |g: scala.Int
         |h: scala.Predef.String
         |o: scala.Option
         |s: SingleType
         |""".stripMargin,
    )
  }

  test("resultClass") {
    assertNoDiff(
      methods(_.resultClass.path),
      """|v: scala.Int
         |w: scala.Int
         |w_$eq: scala.Unit
         |<init>: a.C
         |<init>: a.C
         |f: scala.package.List
         |g: scala.Int
         |h: scala.Predef.String
         |o: scala.Option
         |s: <no symbol>
         |""".stripMargin,
    )
  }

  test("primary constructor") {
    val infos = symtab(convert(classes))
    val printerSymtab: PrinterSymtab = infos.get
    val obtained = infos.values.filter(_.symbol.startsWith("a/C#`<init>`"))
      .map(Print.info(Format.Compact, _, printerSymtab)).toSeq.sorted.mkString("\n")
    assertNoDiff(
      obtained,
      """|a/C#`<init>`(). => primary ctor <init>(val v: Int, var w: Int, x: Int)
         |a/C#`<init>`().(v) => val param v: Int
         |a/C#`<init>`().(w) => var param w: Int
         |a/C#`<init>`().(x) => param x: Int
         |a/C#`<init>`(+1). => ctor <init>(y: Int)
         |a/C#`<init>`(+1).(y) => param y: Int
         |""".stripMargin,
    )
  }

}
