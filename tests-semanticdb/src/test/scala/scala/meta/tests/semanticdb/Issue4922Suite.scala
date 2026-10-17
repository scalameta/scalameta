package scala.meta.tests.semanticdb

import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.tests.metacp.Library

import java.nio.file.Files

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

import munit.FunSuite

class Issue4922Suite extends FunSuite {

  // compile with the semanticdb plugin; metacp then reads the classfiles
  private val classdir = {
    val out = Files.createTempDirectory("issue4922_")
    out.toFile.deleteOnExit()
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.outputDirs.setSingleOutput(out.toString)
    val plugin = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    settings.processArgumentString(
      s"-Yrangepos -Xplugin:$plugin -Xplugin-require:semanticdb -P:semanticdb:failures:error " +
        s"-P:semanticdb:sourceroot:$out -P:semanticdb:targetroot:$out",
    )
    val reporter = new StoreReporter()
    val g = new Global(settings, reporter)
    val code =
      """|package p
         |import scala.annotation.meta._
         |class sa extends scala.annotation.StaticAnnotation
         |class sb extends scala.annotation.StaticAnnotation
         |class K(
         |    @sa val plain: Int,
         |    @(sa @field) val f: Int,
         |    @(sa @getter) val g: Int,
         |    @(sa @param) val p: Int,
         |    @(sa @field @getter) val fg: Int,
         |    @sa var plainVar: Int,
         |    @(sa @field) var fVar: Int,
         |    @(sa @setter) var s: Int,
         |    @(sa @field) h: Int,
         |    @(sa @getter) @(sb @field) val gf: Int,
         |) {
         |  def useH = h
         |  @sa val body = 1
         |  @(sa @getter) val bodyGetter = 1
         |  @sa var bodyVar = 1
         |  @(sa @setter) var bodySetter = 1
         |  @sa @sa val bodyTwice = 1
         |  @(sa @field @getter) @(sa @field @getter) val bodyFgTwice = 1
         |  @(sa @getter) @(sb @field) val bodyGf = 1
         |  @(sa @getter) @sa val bodyGetterPlain = 1
         |}
         |trait T {
         |  @sa val traitVal = 1
         |  @(sa @getter) val traitGetter = 1
         |}
         |class L {
         |  @sa lazy val lz = 1
         |  @(sa @field) lazy val lzField = 1
         |  @(sa @getter) lazy val lzGetter = 1
         |  @sa private[this] val pt = 1
         |  def usePt = pt
         |  @(sa @field) private val pv = 1
         |  @deprecated("", "") val dep = 1
         |  @deprecated("", "") var depVar = 1
         |  @scala.beans.BeanProperty var bean = 1
         |  @volatile var vol = 1
         |  @transient val tr = 1
         |  @sa val (pa, pb) = (1, 2)
         |  @(sa @field) val List(pc) = List(1)
         |  @(sa @field @getter) @(sa @field) val fgThenF = 1
         |  @(sa @getter) @(sa @field) val gThenF = 1
         |  @(sa @field) @(sa @getter @field) val fThenGf = 1
         |  @(sa @field) val + = 1
         |  @(sa @field) val `a b` = 1
         |}
         |abstract class A {
         |  @sa val abs: Int
         |  @(sa @field) val absField: Int
         |  @sa val ov = 1
         |  @sa def defAbs: Int
         |}
         |class B extends A {
         |  val abs = 1
         |  val absField = 1
         |  @(sa @field) override val ov = 2
         |  @(sa @field) val defAbs = 3
         |}
         |object O {
         |  @sa val objVal = 1
         |  @(sa @field) var objVar = 1
         |}
         |trait T2 {
         |  @(sa @field) val traitField = 1
         |  @(sa @field) var traitVar = 1
         |  @(sa @field) lazy val traitLazy = 1
         |}
         |case class CC(@(sa @field) x: Int, @sa y: Int)
         |class V(@(sa @field) val x: Int) extends AnyVal
         |class Outer { class Inner { @sa val inner = 1 } }
         |""".stripMargin
    val src = out.resolve("Issue4922.scala")
    Files.write(src, code.getBytes("UTF-8"))
    new g.Run().compile(List(src.toString))
    assertEquals(reporter.infos.toList.filter(_.severity == reporter.ERROR).map(_.msg), Nil)
    out
  }

  private val metac: Map[String, s.SymbolInformation] = (for {
    file <- FileIO.listAllFilesRecursively(AbsolutePath(classdir)).iterator
    if file.toNIO.toString.endsWith(".semanticdb")
    doc <- TextDocuments.parseFrom(file.readAllBytes).documents
    info <- doc.symbols
  } yield info.symbol -> info).toMap

  private val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++ Classpath(AbsolutePath(classdir)),
    includeJdk = true,
  )

  private def render(tpe: s.Type): String = tpe match {
    case t: s.TypeRef => t.symbol
    case t: s.AnnotatedType => t.annotations.map(x => s" @${render(x.tpe)}")
        .mkString(s"(${render(t.tpe)}", "", ")")
    case t => t.toString
  }

  private def annotations(info: Option[s.SymbolInformation]): String = info
    .fold("<absent>")(_.annotations.map(x => "@" + render(x.tpe)).mkString(" "))

  private def check(symbol: String, metacExpected: String, metacpExpected: String)(implicit
      loc: munit.Location,
  ): Unit = {
    assertEquals(annotations(metac.get(symbol)), metacExpected, "metac")
    assertEquals(annotations(symtab.info(symbol)), metacpExpected, "metacp")
  }

  private def check(symbol: String, expected: String)(implicit loc: munit.Location): Unit =
    check(symbol, expected, expected)

  private val plain = "@p/sa#"
  private val field = "@(p/sa# @scala/annotation/meta/field#)"
  private val getter = "@(p/sa# @scala/annotation/meta/getter#)"
  private val param = "@(p/sa# @scala/annotation/meta/param#)"
  private val setter = "@(p/sa# @scala/annotation/meta/setter#)"
  private val sbField = "@(p/sb# @scala/annotation/meta/field#)"
  private val fieldGetter = "@(p/sa# @scala/annotation/meta/field# @scala/annotation/meta/getter#)"
  private val getterField = "@(p/sa# @scala/annotation/meta/getter# @scala/annotation/meta/field#)"

  test("class parameter val") {
    check("p/K#plain.", "")
    check("p/K#`<init>`().(plain)", plain)
  }

  test("class parameter val with @field") {
    check("p/K#f.", "")
    check("p/K#`<init>`().(f)", "")
  }

  test("class parameter val with @getter") {
    check("p/K#g.", getter)
    check("p/K#`<init>`().(g)", "")
  }

  test("class parameter val with @param") {
    check("p/K#p.", "")
    check("p/K#`<init>`().(p)", param)
  }

  test("class parameter val with @field and @getter") {
    check("p/K#fg.", fieldGetter)
    check("p/K#`<init>`().(fg)", "")
  }

  test("class parameter val with @getter and another @field") {
    check("p/K#gf.", getter)
    check("p/K#`<init>`().(gf)", "")
  }

  test("class parameter var") {
    check("p/K#plainVar().", "")
    check("p/K#`plainVar_=`().", "")
    check("p/K#`plainVar_=`().(x$1)", "")
    check("p/K#`<init>`().(plainVar)", plain)
  }

  test("class parameter var with @field") {
    check("p/K#fVar().", "")
    check("p/K#`fVar_=`().", "")
    check("p/K#`fVar_=`().(x$1)", "")
    check("p/K#`<init>`().(fVar)", "")
  }

  test("class parameter var with @setter") {
    check("p/K#s().", "")
    check("p/K#`s_=`().", setter)
    check("p/K#`s_=`().(x$1)", "")
    check("p/K#`<init>`().(s)", "")
  }

  test("class parameter with @field, used in a method") {
    check("p/K#h.", field)
    check("p/K#`<init>`().(h)", "")
  }

  test("val in the body")(check("p/K#body.", ""))
  test("val in the body with @getter")(check("p/K#bodyGetter.", getter))

  test("val in the body with @getter and another @field")(check("p/K#bodyGf.", getter))
  test("val in the body with @getter and a plain one")(check("p/K#bodyGetterPlain.", getter))

  test("var in the body") {
    check("p/K#bodyVar().", "")
    check("p/K#`bodyVar_=`().", "")
    check("p/K#`bodyVar_=`().(x$1)", "")
  }

  test("var in the body with @setter") {
    check("p/K#bodySetter().", "")
    check("p/K#`bodySetter_=`().", setter)
    check("p/K#`bodySetter_=`().(x$1)", "")
  }

  test("val in the body with @sa twice")(check("p/K#bodyTwice.", ""))

  test("val in the body with @field and @getter twice")(
    check("p/K#bodyFgTwice.", s"$fieldGetter $fieldGetter", fieldGetter),
  )

  test("val in a trait")(check("p/T#traitVal.", plain))
  test("val in a trait with @getter")(check("p/T#traitGetter.", getter))

  test("lazy val in the body")(check("p/L#lz.", plain))
  test("lazy val in the body with @field")(check("p/L#lzField.", field))
  test("lazy val in the body with @getter")(check("p/L#lzGetter.", getter))
  test("private[this] val in the body")(check("p/L#pt.", plain))
  test("private val in the body with @field")(check("p/L#pv.", ""))
  test("val in the body with @deprecated")(check("p/L#dep.", "@scala/deprecated#"))

  test("var in the body with @deprecated") {
    check("p/L#depVar().", "@scala/deprecated#")
    check("p/L#`depVar_=`().", "@scala/deprecated#")
  }

  test("var in the body with @BeanProperty")(check("p/L#bean().", ""))
  test("var in the body with @volatile")(check("p/L#vol().", ""))
  test("val in the body with @transient")(check("p/L#tr.", ""))

  test("tuple pattern val in the body") {
    check("p/L#pa.", "")
    check("p/L#pb.", "")
  }

  test("extractor pattern val in the body with @field")(check("p/L#pc.", ""))

  test("val in the body with @field and @getter, then @field")(check("p/L#fgThenF.", fieldGetter))

  test("val in the body with @getter, then @field")(check("p/L#gThenF.", getter))

  test("val in the body with @field, then @getter and @field")(check("p/L#fThenGf.", getterField))

  test("val with an operator name in the body with @field")(check("p/L#`+`.", ""))
  test("val with a space in its name in the body with @field")(check("p/L#`a b`.", ""))

  test("abstract val") {
    check("p/A#abs.", "")
    check("p/A#absField.", "")
    check("p/B#abs.", "")
    check("p/B#absField.", "")
  }

  test("overriding val with @field") {
    check("p/A#ov.", "")
    check("p/B#ov.", "")
  }

  test("val with @field implementing an abstract def") {
    check("p/A#defAbs().", plain)
    check("p/B#defAbs.", "")
  }

  test("val in an object")(check("p/O.objVal.", ""))
  test("var in an object with @field")(check("p/O.objVar().", ""))

  test("vals in a trait with @field") {
    check("p/T2#traitField.", field)
    check("p/T2#traitVar().", field)
    check("p/T2#traitLazy.", field)
  }

  test("case class parameter") {
    check("p/CC#x.", "")
    check("p/CC#y.", "")
    check("p/CC#`<init>`().(y)", plain)
  }

  test("value class parameter with @field")(check("p/V#x.", ""))
  test("val in an inner class")(check("p/Outer#Inner#inner.", ""))

}
