package scala.meta.tests.semanticdb

import scala.meta.cli.Metacp
import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.tests.cli.CliTestUtils
import scala.meta.tests.metacp.Library

import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1672
class AnnotationArgumentsSuite extends FunSuite {

  // metac reads the semanticdb files of this compilation, metacp its classfiles
  private val classdir = {
    val out = Files.createTempDirectory("issue1672_")
    out.toFile.deleteOnExit()
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.outputDirs.setSingleOutput(out.toString)
    val plugin = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    settings.processArgumentString(
      s"-Yrangepos -Xplugin:$plugin -Xplugin-require:semanticdb -P:semanticdb:failures:error " +
        "-P:semanticdb:synthetics:on " +
        s"-P:semanticdb:sourceroot:$out -P:semanticdb:targetroot:$out",
    )
    val reporter = new StoreReporter()
    val g = new Global(settings, reporter)
    val code =
      """|package p
         |import scala.annotation.meta.field
         |class sa(x: Int, y: String) extends scala.annotation.StaticAnnotation
         |class sd(x: Int, y: String = "d") extends scala.annotation.StaticAnnotation
         |class sc(c: Class[_]) extends scala.annotation.StaticAnnotation
         |class sr(xs: Array[Int]) extends scala.annotation.StaticAnnotation
         |class sn(n: sa) extends scala.annotation.StaticAnnotation
         |class sb extends scala.annotation.StaticAnnotation
         |class sl(l: Long, s: Short) extends scala.annotation.StaticAnnotation
         |class sbc(b: Byte, c: Char) extends scala.annotation.StaticAnnotation
         |class sf(f: Float, d: Double) extends scala.annotation.StaticAnnotation
         |class sdn(x: Int = -1) extends scala.annotation.StaticAnnotation
         |class sk(k: Any) extends scala.annotation.StaticAnnotation
         |class G[T]
         |object C2 {
         |  final val I = -200
         |  final val L = -200L
         |  final val F = Float.NegativeInfinity
         |  final val D = Double.NegativeInfinity
         |  final val DF = -1.5
         |}
         |object C { final val I = 1; final val S = "s"; val v = 3; final val N = -1 }
         |object C3 {
         |  final val Ch = '\uffff'
         |  final val LM = Long.MinValue
         |  final val DZ = -0.0
         |}
         |@sa(1, "a") class K(@sa(1, "a") val f: Int, @(sa @field)(1, "a") val g: Int) {
         |  @sa(1, "a") def literal = 1
         |  @sa(-1, "a" + "b") def folded = 1
         |  @sl(-129L, -2) def negative = 1
         |  @sa(C.I, C.S) def constant = 1
         |  @sa(C.I + 1, C.S + "t") def constantFolded = 1
         |  @sa(C.v, "a") def nonConstant = 1
         |  @sa(y = "b", x = 2) def named = 1
         |  @sa(2, y = "b") def mixed = 1
         |  @sd(1) def default = 1
         |  @sd(y = "e", x = 1) def defaultNamed = 1
         |  @sc(classOf[String]) def clazz = 1
         |  @sr(Array(1, 2)) def array = 1
         |  @sn(new sa(1, "a")) def nested = 1
         |  @sb def noArgs = 1
         |  @deprecated("msg", "1") def scalaPositional = 1
         |  @deprecated("msg", since = "1") def scalaNamed = 1
         |  @throws[Exception]("cause") def throws = 1
         |  @SerialVersionUID(5L) class Serial
         |  @SuppressWarnings(Array("a", "b")) def javaArray = 1
         |  @java.lang.annotation.Retention(java.lang.annotation.RetentionPolicy.RUNTIME)
         |  def javaEnum = 1
         |  @issue1672.Outer(value = new issue1672.Inner(value = "i")) def javaNested = 1
         |  def param(@sa(1, "a") p: Int) = p
         |  @sa(1, "a") type Alias = Int
         |  def tpe: Int @sa(1, "a") = 1
         |  def tpeAssoc: Int @deprecated("msg", "1") = 1
         |  def tpeJava: Int @SuppressWarnings(Array("a")) = 1
         |  def typeParam[@sa(1, "a") T] = 1
         |  @sa(1, "a") val field = 1
         |  @sbc(-1, Char.MaxValue) def byteChar = 1
         |  @sf(Float.NegativeInfinity, Double.NegativeInfinity) def infinite = 1
         |  @sf(-1.5f, -2.5) def finiteNegative = 1
         |  @sdn() def defaultNegative = 1
         |  def tpeNested: Int @issue1672.Outer(value = new issue1672.Inner(value = "j")) = 1
         |  @issue1672.Colors(Array(issue1672.Color.RED)) def missingArray = 1
         |  @issue1672.Wrap(new issue1672.Tint(issue1672.Color.RED)) def missingNested = 1
         |  @sc(classOf[issue1672.Color]) def missingClass = 1
         |  def tpeMissing: Int @issue1672.Colors(Array(issue1672.Color.RED)) = 1
         |  @issue1672.Ints(Array(-1, -129)) def javaInts = 1
         |  @issue1672.NumWrap(value = new issue1672.Num(value = -129)) def javaNumNested = 1
         |  def tpeNegative: Int @sl(-129L, -2) = 1
         |  @issue1672.Shade(value = issue1672.Color.RED) def missingScalar = 1
         |  @sb @sb def twiceNoArgs = 1
         |  @sa(1, "a") @sa(1, "a") def twiceArgs = 1
         |  @sn(new sa(1, "a")) @sn(new sa(1, "a")) def twiceNested = 1
         |  @sb @sa(1, "a") @sb def twiceApart = 1
         |  @deprecated("msg", "1") @deprecated("msg", "1") def twiceScala = 1
         |  def tpeTwice: Int @sb @sb = 1
         |  @sa(K.kv, "a") def companionVal = 1
         |  @sa(K.kd(1), "a") def companionCall = 1
         |  @sa(K.ko("x"), "a") def companionOverload = 1
         |  @sn(n = new sa(1, "a")) def nestedNamed = 1
         |  @sa(math.max(1, 2), "a") def overloaded = 1
         |  @sa(identity[Int](1), "a") def typeApply = 1
         |  @sk(this) def self = 1
         |  @sa({ val z = 2; z }, "a") def block = 1
         |  @sa(if (C.v > 0) 1 else 2, "a") def ifArg = 1
         |  @sa(1, s"a${C.v}") def interpolated = 1
         |  @sk((x: Int) => x) def lambda = 1
         |  class KI(i: Int)
         |  @sk(new KI(1)) def newInner = 1
         |  @sk(K.kn[Int]) def typeApplyCompanion = 1
         |  @sk(K.kt[Int](1)) def typeApplyCall = 1
         |  @sk(K.kc(1)(2)) def curried = 1
         |  @sk(K.kd(K.kv)) def companionNested = 1
         |  @sk(K.kd(C.v)) def companionMixed = 1
         |  @sk(K.KO.f(1)) def nestedObject = 1
         |  @sk(K.ks(1).kd(2)) def applyQualifier = 1
         |  def dependent(x: Int): Int @sk(x) = x
         |  @sk(K.kn[issue1672.Color]) def missingTypeArg = 1
         |  implicit final val n = 1
         |  def implicitArg(implicit i: Int) = i
         |  def implicitUse = implicitArg
         |  def tpeNamed: Int @sa(y = "f", x = 3) = 1
         |  def paramNamed(@sa(y = "g", x = 4) p: Int) = p
         |  def inferredNamed(p: Int @sa(y = "h", x = 5)) = p
         |  def inferredLocal = { val v: Int @sa(y = "i", x = 6) = 1; v }
         |  def inferredLambda = (q: Int @sa(y = "j", x = 7)) => q
         |  def inferredPoly[T](t: T @sa(y = "k", x = 8)) = t
         |  def inferredNested(p: List[Int @sa(y = "l", x = 9)]) = p
         |  def inferredTwoAnns(p: Int @sa(y = "m", x = 10) @sa(y = "n", x = 11)) = p
         |  val inferredVal = { val v: Int @sa(y = "o", x = 12) = 1; v }
         |  def localCopy = { val v: Int @sa(y = "p", x = 13) = 1; val w = v; def d = w; d }
         |  def localParam = { def d(p: Int @sa(y = "q", x = 14)) = { val c = p; c }; d(1) }
         |  def localClassParam = { class L(p: Int @sa(y = "r", x = 15)) { val c = p }; new L(1).c }
         |  def localLambda = { val f = (p: Int @sa(y = "s", x = 16)) => p; f }
         |  def ascribed = (1: @sa(y = "t", x = 17))
         |  val ascribedVal = (1: @sa(y = "u", x = 18))
         |  def localAscribed = { val v = (1: @sa(y = "v", x = 19)); v }
         |  def typeArg = identity[Int @sa(y = "w", x = 20)](1)
         |  def newTypeArg = new G[Int @sa(y = "x", x = 21)]
         |  class ParentArg extends G[Int @sa(y = "y", x = 22)]
         |  def boundArg[T <: G[Int @sa(y = "z", x = 23)]] = 1
         |  type AliasArg = G[Int @sa(y = "a", x = 24)]
         |  def patternArg(a: Any) = a match { case l: List[Int @sa(y = "b", x = 25)] => l }
         |  def typeArgList = List[Int @sa(y = "c", x = 26)](1)
         |}
         |object K {
         |  val kv = 2
         |  def kd(i: Int) = i
         |  def ko(i: Int) = i
         |  def ko(s: String) = 1
         |  @sk(this) def selfObject = 1
         |  def kn[T] = 1
         |  def kt[T](t: T) = 1
         |  def kc(a: Int)(b: Int) = a
         |  def ks(i: Int): K.type = K
         |  object KO { def f(i: Int) = i }
         |}
         |""".stripMargin +
        (if (ScalaVersion.is212) ""
         else "object C4 { def lt: -1 = -1; def lo: Option[-129L] = None }\n")
    val src = out.resolve("Issue1672.scala")
    Files.write(src, code.getBytes("UTF-8"))
    new g.Run().compile(List(src.toString))
    assertEquals(reporter.infos.toList.filter(_.severity == reporter.ERROR).map(_.msg), Nil)
    out
  }

  private val documents = (for {
    file <- FileIO.listAllFilesRecursively(AbsolutePath(classdir)).iterator
    if file.toNIO.toString.endsWith(".semanticdb")
    doc <- TextDocuments.parseFrom(file.readAllBytes).documents
  } yield doc).toList

  private val metac: Map[String, s.SymbolInformation] = documents.iterator
    .flatMap(_.symbols.map(info => info.symbol -> info)).toMap

  // the directory of the Java annotations that the code uses
  private val javaClasses = Classpath(AbsolutePath(
    Paths.get(classOf[issue1672.Outer].getProtectionDomain.getCodeSource.getLocation.toURI),
  ))

  private val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++ Classpath(AbsolutePath(classdir)) ++ javaClasses,
    includeJdk = true,
  )

  // the Java annotations without `Color` and `Tint`, which the arguments of some refer to
  private val javaMissing = {
    val dir = Files.createTempDirectory("issue1672_java_")
    dir.toFile.deleteOnExit()
    val pkg = javaClasses.entries.head.toNIO.resolve("issue1672")
    Files.createDirectories(dir.resolve("issue1672"))
    Files.list(pkg).iterator().asScala.map(_.getFileName.toString)
      .filterNot(x => x == "Color.class" || x == "Tint.class")
      .foreach(x => Files.copy(pkg.resolve(x), dir.resolve("issue1672").resolve(x)))
    Classpath(AbsolutePath(dir))
  }

  private val symtabMissing = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++ Classpath(AbsolutePath(classdir)) ++ javaMissing,
    includeJdk = true,
  )

  // whether metacp converts the code without `Color` and `Tint`, and what it reports
  private def metacpMissing(stub: Boolean, log: Boolean): String = {
    val out = Files.createTempDirectory("issue1672_metacp_")
    out.toFile.deleteOnExit()
    val settings = scala.meta.metacp.Settings().withOut(AbsolutePath(out))
      .withClasspath(Classpath(AbsolutePath(classdir)))
      .withDependencyClasspath(Library.scalaLibrary.classpath() ++ javaMissing).withIncludeJdk(true)
      .withStubBrokenSignatures(stub).withLogBrokenSignatures(log)
    val (result, _, err) = CliTestUtils.withReporter(reporter => Metacp.process(settings, reporter))
    val errors = err.linesIterator.filter(x => x.startsWith("broken") || x.startsWith("missing"))
      .map(_.replaceAll(" in .*", "")).toList.distinct.sorted
    (s"success = ${result.isSuccess}" :: errors).mkString("\n")
  }

  private def render(tree: s.Tree): String = tree match {
    case t: s.AnnotationTree => "@" + render(t.tpe) + t.arguments.map(render)
        .mkString("(", ", ", ")")
    case t: s.ApplyTree => render(t.function) + t.arguments.map(render).mkString("(", ", ", ")")
    case t: s.AssignTree => render(t.lhs) + " = " + render(t.rhs)
    case t: s.IdTree => t.symbol
    case t: s.LiteralTree => s.Print.constant(t.constant)
    case t: s.SelectTree => s"sel(${render(t.qualifier)}, ${t.id.fold("<?>")(render)})"
    case t: s.TypeApplyTree => render(t.function) + t.typeArguments.map(render)
        .mkString("[", ", ", "]")
    case _: s.OriginalTree => "*"
    case s.NoTree => "<?>"
    case t => t.toString
  }

  private def render(tpe: s.Type): String = tpe match {
    case t: s.ConstantType => s.Print.constant(t.constant)
    case t: s.TypeRef if t.typeArguments.isEmpty => t.symbol
    case t: s.TypeRef => t.symbol + t.typeArguments.map(render).mkString("[", ", ", "]")
    case t: s.AnnotatedType => t.annotations.map(x => s" @${render(x.tpe)}")
        .mkString(s"(${render(t.tpe)}", "", ")")
    case t => t.toString
  }

  private def annotations(info: Option[s.SymbolInformation]): String = info.fold("<absent>") { i =>
    val tpe = i.signature match {
      case s.MethodSignature(_, _, t: s.AnnotatedType, _) => t.annotations
      case _ => Nil
    }
    (i.annotations ++ tpe).map(render).mkString(" ")
  }

  private def check(symbol: String, metacExpected: String, metacpExpected: String)(implicit
      loc: munit.Location,
  ): Unit = {
    assertEquals(annotations(metac.get(symbol)), metacExpected, "metac")
    assertEquals(annotations(symtab.info(symbol)), metacpExpected, "metacp")
  }

  private def check(symbol: String, expected: String)(implicit loc: munit.Location): Unit =
    check(symbol, expected, expected)

  // metacp without the classes that the arguments refer to
  private def checkMissing(symbol: String, expected: String)(implicit loc: munit.Location): Unit =
    assertEquals(annotations(symtabMissing.info(symbol)), expected)

  private def returnType(info: Option[s.SymbolInformation]): String = info.map(_.signature) match {
    case Some(s.MethodSignature(_, _, s.ConstantType(c), _)) => s.Print.constant(c)
    case Some(s.MethodSignature(_, _, t, _)) => render(t)
    case sig => sig.toString
  }

  // the annotations in a type, with `forSome` for an existential type
  private def annotationsIn(tpe: s.Type): String = tpe match {
    case t: s.ExistentialType => "forSome " + annotationsIn(t.tpe)
    case t: s.AnnotatedType => t.annotations.map(render).mkString(" ")
    case t: s.TypeRef => t.typeArguments.map(annotationsIn).mkString(" ")
    case _ => ""
  }

  private def inferred(info: Option[s.SymbolInformation]): String = info.map(_.signature) match {
    case Some(x: s.MethodSignature) => annotationsIn(x.returnType)
    case sig => sig.toString
  }

  // the annotations in a declared type: the parents of a class, the upper bound of a type
  private def declared(info: Option[s.SymbolInformation]): String = info.map(_.signature) match {
    case Some(x: s.ClassSignature) => x.parents.map(annotationsIn).mkString(" ")
    case Some(x: s.TypeSignature) => annotationsIn(x.upperBound)
    case sig => sig.toString
  }

  private def checkIn(
      read: Option[s.SymbolInformation] => String,
      symbol: String,
      metacExpected: String,
      metacpExpected: String,
  )(implicit loc: munit.Location): Unit = {
    assertEquals(read(metac.get(symbol)), metacExpected, "metac")
    assertEquals(read(symtab.info(symbol)), metacpExpected, "metacp")
  }

  // before 2.13.17, scalac keeps a positional default argument as a call to its getter
  private val defaultGetter = !ScalaVersion.isSupported(minimal212 = Int.MaxValue, minimal213 = 17)

  test("class")(check("p/K#", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))
  test("literal arguments")(check("p/K#literal().", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))
  test("folded literals")(check("p/K#folded().", """@p/sa#(-1, "ab")""", """@p/sa#(-1, "ab")"""))
  test("negative long and short")(
    check("p/K#negative().", """@p/sl#(-129L, -2)""", """@p/sl#(-129L, -2)"""),
  )
  test("constant arguments")(check("p/K#constant().", """@p/sa#(1, "s")""", """@p/sa#(1, "s")"""))

  test("folded constants")(
    check("p/K#constantFolded().", """@p/sa#(2, "st")""", """@p/sa#(2, "st")"""),
  )

  test("non-constant argument")(
    check("p/K#nonConstant().", """@p/sa#(p/C.v., "a")""", """@p/sa#(<?>, "a")"""),
  )

  test("named arguments")(check(
    "p/K#named().",
    """@p/sa#(2, "b")""",
    if (ScalaVersion.is212) """@p/sa#(<?>, <?>)""" else """@p/sa#(2, "b")""",
  ))

  test("positional and named arguments")(
    check("p/K#mixed().", """@p/sa#(2, "b")""", """@p/sa#(2, "b")"""),
  )

  test("default argument")(check(
    "p/K#default().",
    if (defaultGetter) """@p/sd#(1, p/sd.`<init>$default$2`().)""" else """@p/sd#(1, "d")""",
    if (defaultGetter) """@p/sd#(1, <?>)""" else """@p/sd#(1, "d")""",
  ))

  test("named arguments with a default")(check(
    "p/K#defaultNamed().",
    """@p/sd#(1, "e")""",
    if (ScalaVersion.is212) """@p/sd#(<?>, <?>)""" else """@p/sd#(1, "e")""",
  ))

  test("return type, named arguments")(check(
    "p/K#tpeNamed().",
    """@p/sa#(3, "f")""",
    if (ScalaVersion.is212) """@p/sa#(<?>, <?>)""" else """@p/sa#(3, "f")""",
  ))

  test("method parameter, named arguments")(check(
    "p/K#paramNamed().(p)",
    """@p/sa#(4, "g")""",
    if (ScalaVersion.is212) """@p/sa#(<?>, <?>)""" else """@p/sa#(4, "g")""",
  ))

  test("type argument of a call, named arguments")(checkIn(inferred, "p/K#typeArg().", "", ""))

  test("type argument of a list call, named arguments")(checkIn(
    inferred,
    "p/K#typeArgList().",
    if (ScalaVersion.is212) """forSome @p/sa#(26, "c")""" else """@p/sa#(26, "c")""",
    if (ScalaVersion.is212) """forSome @p/sa#(<?>, <?>)""" else """@p/sa#(26, "c")""",
  ))

  test("type argument of a new, named arguments")(checkIn(
    inferred,
    "p/K#newTypeArg().",
    if (ScalaVersion.is212) """forSome @p/sa#(21, "x")""" else """@p/sa#(21, "x")""",
    if (ScalaVersion.is212) """forSome @p/sa#(<?>, <?>)""" else """@p/sa#(21, "x")""",
  ))

  test("type argument of a parent, named arguments")(checkIn(
    declared,
    "p/K#ParentArg#",
    """@p/sa#(22, "y")""",
    if (ScalaVersion.is212) """@p/sa#(<?>, <?>)""" else """@p/sa#(22, "y")""",
  ))

  test("type argument of an upper bound, named arguments")(checkIn(
    declared,
    "p/K#boundArg().[T]",
    """@p/sa#(23, "z")""",
    if (ScalaVersion.is212) """@p/sa#(<?>, <?>)""" else """@p/sa#(23, "z")""",
  ))

  test("type argument of a type alias, named arguments")(checkIn(
    declared,
    "p/K#AliasArg#",
    """@p/sa#(24, "a")""",
    if (ScalaVersion.is212) """@p/sa#(<?>, <?>)""" else """@p/sa#(24, "a")""",
  ))

  test("type argument of a type pattern, named arguments")(checkIn(
    inferred,
    "p/K#patternArg().",
    if (ScalaVersion.is212) """forSome @p/sa#(25, "b")""" else """@p/sa#(25, "b")""",
    if (ScalaVersion.is212) """forSome @p/sa#(<?>, <?>)""" else """@p/sa#(25, "b")""",
  ))

  test("inferred return type, named arguments") {
    assertEquals(
      inferred(metac.get("p/K#inferredNamed().")),
      if (ScalaVersion.is212) """forSome @p/sa#(5, "h")""" else """@p/sa#(5, "h")""",
      "metac",
    )
    assertEquals(
      inferred(symtab.info("p/K#inferredNamed().")),
      if (ScalaVersion.is212) "forSome @p/sa#(<?>, <?>)" else """@p/sa#(5, "h")""",
      "metacp",
    )
  }

  test("inferred type of a local value, named arguments")(assertEquals(
    inferred(metac.get("p/K#inferredLocal().")),
    if (ScalaVersion.is212) """forSome @p/sa#(6, "i")""" else """@p/sa#(6, "i")""",
  ))

  test("inferred type of a lambda, named arguments")(assertEquals(
    inferred(metac.get("p/K#inferredLambda().")),
    if (ScalaVersion.is212) """forSome @p/sa#(7, "j") forSome @p/sa#(7, "j")"""
    else """@p/sa#(7, "j") @p/sa#(7, "j")""",
  ))

  test("inferred return type of a polymorphic method, named arguments")(assertEquals(
    inferred(metac.get("p/K#inferredPoly().")),
    if (ScalaVersion.is212) """forSome @p/sa#(8, "k")""" else """@p/sa#(8, "k")""",
  ))

  test("inferred type argument, named arguments")(assertEquals(
    inferred(metac.get("p/K#inferredNested().")),
    if (ScalaVersion.is212) """forSome @p/sa#(9, "l")""" else """@p/sa#(9, "l")""",
  ))

  test("inferred type, two annotations with named arguments")(assertEquals(
    inferred(metac.get("p/K#inferredTwoAnns().")),
    if (ScalaVersion.is212) """forSome @p/sa#(10, "m") @p/sa#(11, "n")"""
    else """@p/sa#(10, "m") @p/sa#(11, "n")""",
  ))

  test("inferred type of a field, named arguments")(assertEquals(
    inferred(metac.get("p/K#inferredVal.")),
    if (ScalaVersion.is212) """forSome @p/sa#(12, "o")""" else """@p/sa#(12, "o")""",
  ))

  test("inferred type of an ascription, named arguments")(assertEquals(
    inferred(metac.get("p/K#ascribed().")),
    if (ScalaVersion.is212) """forSome @p/sa#(17, "t")""" else """@p/sa#(17, "t")""",
  ))

  test("inferred type of a field with an ascription, named arguments")(assertEquals(
    inferred(metac.get("p/K#ascribedVal.")),
    if (ScalaVersion.is212) """forSome @p/sa#(18, "u")""" else """@p/sa#(18, "u")""",
  ))

  // the symbols with an annotation argument that refers to a local
  test("inferred types of locals, named arguments")(assertEquals(
    documents.flatMap(_.symbols).filter(_.toString.contains("IdTree(local")).map(_.symbol),
    Nil,
  ))

  test("class literal argument")(
    check("p/K#clazz().", """@p/sc#(scala/Predef.String#)""", """@p/sc#(scala/Predef.String#)"""),
  )

  test("array argument")(check(
    "p/K#array().",
    """@p/sr#(sel(scala/Array., scala/Array.apply(+5).)(1, 2))""",
    """@p/sr#(<?>)""",
  ))

  test("nested annotation argument")(
    check("p/K#nested().", """@p/sn#(p/sa#`<init>`().(1, "a"))""", """@p/sn#(<?>)"""),
  )

  test("no arguments")(check("p/K#noArgs().", """@p/sb#()"""))

  test("scala library annotation, positional arguments")(
    if (ScalaVersion.is212) check(
      "p/K#scalaPositional().",
      """@scala/deprecated#("msg", "1")""",
      """@scala/deprecated#("msg", "1")""",
    )
    else check("p/K#scalaPositional().", """@scala/deprecated#(message = "msg", since = "1")"""),
  )

  test("scala library annotation, named argument")(
    if (ScalaVersion.is212) check(
      "p/K#scalaNamed().",
      """@scala/deprecated#("msg", "1")""",
      """@scala/deprecated#("msg", "1")""",
    )
    else check("p/K#scalaNamed().", """@scala/deprecated#(message = "msg", since = "1")"""),
  )

  test("type argument and argument")(check(
    "p/K#throws().",
    """@scala/throws#[scala/package.Exception#]("cause")""",
    """@scala/throws#[scala/package.Exception#]("cause")""",
  ))

  test("class with a long argument")(check("p/K#Serial#", """@scala/SerialVersionUID#(value = 5L)"""))

  test("java annotation, array argument")(check(
    "p/K#javaArray().",
    """@java/lang/SuppressWarnings#(value = <?>("a", "b"))""",
    """@java/lang/SuppressWarnings#(value = <?>("a", "b"))""",
  ))

  test("java annotation, enum argument")(check(
    "p/K#javaEnum().",
    """@java/lang/annotation/Retention#(value = java/lang/annotation/RetentionPolicy#RUNTIME.)""",
  ))

  test("method parameter")(check("p/K#param().(p)", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))
  test("class parameter")(check("p/K#`<init>`().(f)", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))
  test("class parameter accessor")(check("p/K#f.", ""))

  test("class parameter with a field meta-annotation")(
    check("p/K#g.", """@(p/sa# @scala/annotation/meta/field#)(1, "a")"""),
  )

  test("val")(check("p/K#field.", """@p/sa#(1, "a")"""))
  test("type alias")(check("p/K#Alias#", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))
  test("type parameter")(check("p/K#typeParam().[T]", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))
  test("return type")(check("p/K#tpe().", """@p/sa#(1, "a")""", """@p/sa#(1, "a")"""))

  test("return type, scala library annotation")(
    if (ScalaVersion.is212) check(
      "p/K#tpeAssoc().",
      """@scala/deprecated#("msg", "1")""",
      """@scala/deprecated#("msg", "1")""",
    )
    else check(
      "p/K#tpeAssoc().",
      """@scala/deprecated#(message = "msg", since = "1")""",
      """@scala/deprecated#(message = "msg", since = "1")""",
    ),
  )

  test("return type, java annotation")(check(
    "p/K#tpeJava().",
    """@java/lang/SuppressWarnings#(value = <?>("a"))""",
    """@java/lang/SuppressWarnings#(value = <?>("a"))""",
  ))

  test("negative constant") {
    assertEquals(returnType(metac.get("p/C.N.")), "-1", "metac")
    assertEquals(returnType(symtab.info("p/C.N.")), "-1", "metacp")
  }

  test("java annotation, nested annotation argument")(check(
    "p/K#javaNested().",
    """@issue1672/Outer#(value = @issue1672/Inner#(value = "i"))""",
    """@issue1672/Outer#(value = @issue1672/Inner#(value = "i"))""",
  ))

  test("byte and char arguments")(
    check("p/K#byteChar().", "@p/sbc#(-1, '\uffff')", "@p/sbc#(-1, '\uffff')"),
  )

  test("infinite float and double arguments")(
    check("p/K#infinite().", "@p/sf#(-Infinityf, -Infinity)", "@p/sf#(-Infinityf, -Infinity)"),
  )

  test("negative float and double arguments")(
    check("p/K#finiteNegative().", "@p/sf#(-1.5f, -2.5)", "@p/sf#(-1.5f, -2.5)"),
  )

  test("negative default argument")(check(
    "p/K#defaultNegative().",
    if (defaultGetter) "@p/sdn#(p/sdn.`<init>$default$1`().)" else "@p/sdn#(-1)",
    if (defaultGetter) "@p/sdn#(<?>)" else "@p/sdn#(-1)",
  ))

  test("return type, nested java annotation")(check(
    "p/K#tpeNested().",
    "@issue1672/Outer#(value = @issue1672/Inner#(value = \"j\"))",
    "@issue1672/Outer#(value = @issue1672/Inner#(value = \"j\"))",
  ))

  test("negative constants") {
    assertEquals(
      List("I", "L", "F", "D", "DF").map(x => returnType(metac.get(s"p/C2.$x."))),
      List("-200", "-200L", "-Infinityf", "-Infinity", "-1.5"),
      "metac",
    )
    assertEquals(
      List("I", "L", "F", "D", "DF").map(x => returnType(symtab.info(s"p/C2.$x."))),
      List("-200", "-200L", "-Infinityf", "-Infinity", "-1.5"),
      "metacp",
    )
  }

  test("missing class in an array argument")(
    checkMissing("p/K#missingArray().", "@issue1672/Colors#(value = <?>(<?>))"),
  )

  test("missing class in a nested argument")(
    checkMissing("p/K#missingNested().", "@issue1672/Wrap#(value = <?>)"),
  )

  test("missing class in a class literal argument")(
    checkMissing("p/K#missingClass().", "@p/sc#(<?>)"),
  )

  test("return type, missing class in an argument")(
    checkMissing("p/K#tpeMissing().", "@issue1672/Colors#(value = <?>(<?>))"),
  )

  test("implicit constant argument") {
    val lines = Files.readAllLines(classdir.resolve("Issue1672.scala")).asScala
    val line = lines.indexWhere(_.contains("def implicitUse"))
    val synthetics = documents.flatMap(_.synthetics).filter(_.range.exists(_.startLine == line))
      .map(x => render(x.tree))
    assertEquals(synthetics, List("*(1)"))
  }

  test("constant types of other widths") {
    val names = List("Ch", "LM", "DZ")
    assertEquals(
      names.map(x => returnType(metac.get(s"p/C3.$x."))),
      List("'\uffff'", "-9223372036854775808L", "0.0"),
      "metac",
    )
    assertEquals(
      names.map(x => returnType(symtab.info(s"p/C3.$x."))),
      List("'\uffff'", "-9223372036854775808L", "-0.0"),
      "metacp",
    )
  }

  test("negative literal types") {
    assume(!ScalaVersion.is212, "literal types need 2.13")
    val names = List("lt", "lo")
    assertEquals(
      names.map(x => returnType(metac.get(s"p/C4.$x()."))),
      List("-1", "scala/Option#[-129L]"),
      "metac",
    )
    assertEquals(
      names.map(x => returnType(symtab.info(s"p/C4.$x()."))),
      List("-1", "scala/Option#[-129L]"),
      "metacp",
    )
  }

  test("java int array argument")(check(
    "p/K#javaInts().",
    "@issue1672/Ints#(value = <?>(-1, -129))",
    "@issue1672/Ints#(value = <?>(-1, -129))",
  ))

  test("nested java annotation, negative argument")(check(
    "p/K#javaNumNested().",
    "@issue1672/NumWrap#(value = @issue1672/Num#(value = -129))",
    "@issue1672/NumWrap#(value = @issue1672/Num#(value = -129))",
  ))

  test("return type, negative arguments")(
    check("p/K#tpeNegative().", "@p/sl#(-129L, -2)", "@p/sl#(-129L, -2)"),
  )

  test("missing class in a scalar argument")(
    checkMissing("p/K#missingScalar().", "@issue1672/Shade#(value = <?>)"),
  )

  test("metacp without stubs, missing classes")(assertNoDiff(
    metacpMissing(stub = false, log = false),
    """|success = false
       |missing symbol: issue1672.Color
       |""".stripMargin,
  ))

  test("metacp with stubs and logs, missing classes")(assertNoDiff(
    metacpMissing(stub = true, log = true),
    """|success = true
       |broken annotation argument: missing symbol: issue1672.Color
       |broken annotation argument: missing symbol: issue1672.Tint
       |""".stripMargin,
  ))

  test("annotation twice without arguments")(
    check("p/K#twiceNoArgs().", "@p/sb#() @p/sb#()", "@p/sb#()"),
  )

  test("annotation twice with arguments")(check(
    "p/K#twiceArgs().",
    """@p/sa#(1, "a") @p/sa#(1, "a")""",
    """@p/sa#(1, "a") @p/sa#(1, "a")""",
  ))

  test("annotation twice with a nested argument")(check(
    "p/K#twiceNested().",
    """@p/sn#(p/sa#`<init>`().(1, "a")) @p/sn#(p/sa#`<init>`().(1, "a"))""",
    "@p/sn#(<?>) @p/sn#(<?>)",
  ))

  test("annotation twice around another")(
    check("p/K#twiceApart().", """@p/sb#() @p/sa#(1, "a") @p/sb#()""", """@p/sb#() @p/sa#(1, "a")"""),
  )

  test("scala library annotation twice") {
    if (ScalaVersion.is212) check(
      "p/K#twiceScala().",
      """@scala/deprecated#("msg", "1") @scala/deprecated#("msg", "1")""",
      """@scala/deprecated#("msg", "1") @scala/deprecated#("msg", "1")""",
    )
    else check(
      "p/K#twiceScala().",
      """@scala/deprecated#(message = "msg", since = "1") @scala/deprecated#(message = "msg", since = "1")""",
      """@scala/deprecated#(message = "msg", since = "1")""",
    )
  }

  test("return type, annotation twice")(check("p/K#tpeTwice().", "@p/sb#() @p/sb#()"))

  test("companion val argument")(
    check("p/K#companionVal().", """@p/sa#(p/K.kv., "a")""", """@p/sa#(p/K.kv., "a")"""),
  )

  test("companion method call argument")(check(
    "p/K#companionCall().",
    """@p/sa#(sel(p/K., p/K.kd().)(1), "a")""",
    """@p/sa#(sel(p/K., p/K.kd().)(1), "a")""",
  ))

  test("companion overloaded method call argument")(check(
    "p/K#companionOverload().",
    """@p/sa#(sel(p/K., p/K.ko(+1).)("x"), "a")""",
    """@p/sa#(sel(p/K., p/K.ko(+1).)("x"), "a")""",
  ))

  test("named nested annotation argument")(
    check("p/K#nestedNamed().", """@p/sn#(p/sa#`<init>`().(1, "a"))""", "@p/sn#(<?>)"),
  )

  test("overloaded method call argument")(check(
    "p/K#overloaded().",
    """@p/sa#(sel(scala/math/package., scala/math/package.max().)(1, 2), "a")""",
    """@p/sa#(<?>, "a")""",
  ))

  test("type application argument")(
    check("p/K#typeApply().", """@p/sa#(scala/Predef.identity().(1), "a")""", """@p/sa#(<?>, "a")"""),
  )

  test("this argument")(check("p/K#self().", "@p/sk#(p/K#)", "@p/sk#(p/K#)"))
  test("this argument in an object")(check("p/K.selfObject().", "@p/sk#(p/K.)", "@p/sk#(p/K.)"))
  test("block argument")(check("p/K#block().", """@p/sa#(<?>, "a")"""))
  test("if argument")(check("p/K#ifArg().", """@p/sa#(<?>, "a")"""))

  test("interpolated argument")(check(
    "p/K#interpolated().",
    if (ScalaVersion.is212)
      "@p/sa#(1, sel(scala/StringContext.apply()., scala/StringContext#s().)(p/C.v.))"
    else "@p/sa#(1, sel(, java/lang/String#`+`().)(p/C.v.))",
    "@p/sa#(1, <?>)",
  ))

  test("function argument")(check("p/K#lambda().", "@p/sk#(<?>)"))

  test("new inner class argument")(
    check("p/K#newInner().", "@p/sk#(p/K#KI#`<init>`().(1))", "@p/sk#(p/K#KI#`<init>`().(1))"),
  )

  test("companion type application argument")(check(
    "p/K#typeApplyCompanion().",
    "@p/sk#(sel(p/K., p/K.kn().)[scala/Int#])",
    "@p/sk#(sel(p/K., p/K.kn().)[scala/Int#])",
  ))

  test("companion type application call argument")(
    check("p/K#typeApplyCall().", "@p/sk#(p/K.kt().(1))", "@p/sk#(p/K.kt().(1))"),
  )

  test("companion curried call argument")(
    check("p/K#curried().", "@p/sk#(p/K.kc().(2))", "@p/sk#(p/K.kc().(2))"),
  )

  test("companion call with a companion argument")(check(
    "p/K#companionNested().",
    "@p/sk#(sel(p/K., p/K.kd().)(p/K.kv.))",
    "@p/sk#(sel(p/K., p/K.kd().)(p/K.kv.))",
  ))

  test("companion call with an outside argument")(
    check("p/K#companionMixed().", "@p/sk#(sel(p/K., p/K.kd().)(p/C.v.))", "@p/sk#(<?>)"),
  )

  test("nested object call argument")(check(
    "p/K#nestedObject().",
    "@p/sk#(sel(p/K.KO., p/K.KO.f().)(1))",
    "@p/sk#(sel(p/K.KO., p/K.KO.f().)(1))",
  ))

  test("call on a call argument")(check(
    "p/K#applyQualifier().",
    "@p/sk#(sel(p/K.ks()., p/K.kd().)(2))",
    "@p/sk#(sel(p/K.ks()., p/K.kd().)(2))",
  ))

  test("return type, parameter argument")(
    check("p/K#dependent().", "@p/sk#(p/K#dependent().(x))", "@p/sk#(p/K#dependent().(x))"),
  )

  test("missing class in a type application argument")(
    checkMissing("p/K#missingTypeArg().", "@p/sk#(<?>)"),
  )

}
