package scala.meta.tests.semanticdb

import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.scalac.{BinaryMode, SemanticdbOps, SemanticdbPlugin}
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.metap.Format

import java.io.File
import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1591
class LocalSymbolsSuite extends FunSuite {

  // the helpers compile with a new run of one of these
  private lazy val semanticdb = new Compiler("-P:semanticdb:failures:error -P:semanticdb:text:on")
  private lazy val reporting = new Compiler("")
  private lazy val typer = {
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.processArgumentString("-Yrangepos -Ystop-after:typer")
    new Global(settings, new StoreReporter())
  }

  private val defs =
    """|object Defs {
       |  type User = { def name: String }
       |  val u: { def age: Int } = ???
       |  def take(p: { def id: Int }): Int = p.id
       |  class C
       |  val r: C { def y: Int } = ???
       |  type F[T] = { def get: T }
       |  val m: { def poly[T](t: T): T } = ???
       |  val e: List[List[_]] = ???
       |  val lub = if (true) new { def z = 1 } else new { def z = 2 }
       |}
       |""".stripMargin

  private val use =
    """|object Use {
       |  val user: User = ???
       |  val name = user.name
       |  val age = u.age
       |  def f(p: { def id: Int }) = p.id
       |  val viaParam = Option(u).map(p => p.age)
       |  val y = r.y
       |  val get = (??? : F[Int]).get
       |  val poly = m.poly(1)
       |  val z = lub.z
       |  val inferred = u
       |  val inferredR = r
       |  def inferredDef = r
       |  val inferredF = (??? : F[Int])
       |  val inferredM = m
       |  val inferredE = e
       |  val takeFn = take _
       |}
       |""".stripMargin

  // the local `y` of B has the offset of the member `y` of A
  private val offsetA =
    """|package a
       |object Defs {
       |  class C
       |  val r: C { def y: Int } = ???
       |}
       |""".stripMargin

  private val offsetB =
    """|package b
       |object Use {
       |  def sameOffsetAsY = { val y = 1; y }
       |  val inferredR = a.Defs.r
       |}
       |""".stripMargin

  // one owner holds two refinements with the same member names
  private val twinsA =
    """|package a
       |object Twins {
       |  def two(p: { def x: Int }, q: { def x: Int }): Int = p.x + q.x
       |  val pair: ({ def x: Int }, { def x: Int }) = ???
       |}
       |""".stripMargin

  private val twinsB =
    """|package b
       |object UseTwins {
       |  val two = a.Twins.two _
       |  val pair = a.Twins.pair
       |}
       |""".stripMargin

  // one owner holds refinements that share a member name: with other types, and copied
  private val mixedA =
    """|package a
       |object Mixed {
       |  class C
       |  val types: ({ def x: Int }, { def x: String }) = ???
       |  val copies: (C { def x: Int }, C { def x: Int }) = ???
       |}
       |""".stripMargin

  private val mixedB =
    """|package b
       |object UseMixed {
       |  val types = a.Mixed.types
       |  val copies = a.Mixed.copies
       |  val x = a.Mixed.copies._2.x
       |}
       |""".stripMargin

  private val a = "package a\n" + defs
  private val b = "package b\nimport a.Defs._\n" + use
  private val c = "package c\nimport Defs._\n" + defs + use

  // a new run of `g`, without the errors, the top-level symbols and the packages of its last run
  private def newRun(g: Global): g.Run = {
    g.reporter.reset()
    val last = g.currentRun
    // unlink `sym`, and then its package if the package has no other member
    def unlink(sym: g.Symbol): Unit = {
      val owner = sym.owner
      val decls = g.enteringPhase(last.typerPhase)(owner.info.decls)
      decls.unlink(sym)
      if (decls.isEmpty && owner.hasPackageFlag && !owner.isEffectiveRoot) unlink(owner.sourceModule)
    }
    if (last ne null) last.symSource.keys.foreach(unlink)
    new g.Run()
  }

  // a compiler with the plugin and `options`
  private class Compiler(val options: String, classpath: String*) {

    private val reporter = new StoreReporter()
    private val settings = new Settings(msg => fail(msg))
    settings.classpath.value = (sys.props("sbt.paths.tests.test.classes") +: classpath)
      .mkString(File.pathSeparator)
    private val plugin = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    settings
      .processArgumentString(s"-Yrangepos -Xplugin:$plugin -Xplugin-require:semanticdb $options")
    private val g = new Global(settings, reporter)

    // the errors of a run that compiles `sources` into `out`, its sourceroot and targetroot
    def compile(sources: Map[String, String], out: Path): List[StoreReporter.Info] = {
      sources.foreach { case (name, code) =>
        Files.write(out.resolve(name), code.getBytes("UTF-8"))
      }
      settings.outputDirs.setSingleOutput(out.toString)
      reporter.reset()
      val run = newRun(g)
      // the first run loads the plugin
      g.plugins.foreach {
        case p: SemanticdbPlugin =>
          p.config = p.config.copy(sourceroot = AbsolutePath(out), targetroot = AbsolutePath(out))
        case _ =>
      }
      run.compile(sources.keys.toList.sorted.map(out.resolve(_).toString))
      reporter.infos.toList.filter(_.severity == reporter.ERROR)
    }

  }

  private def compile(sources: Map[String, String], classpath: String*): Path = {
    val out = Files.createTempDirectory("issue1591_")
    // each other classpath is a new directory, so a compiler for it runs once
    val compiler =
      if (classpath.isEmpty) semanticdb else new Compiler(semanticdb.options, classpath: _*)
    assertEquals(compiler.compile(sources, out).map(_.msg), Nil)
    out
  }

  private def document(out: Path, name: String): s.TextDocument = FileIO
    .listAllFilesRecursively(AbsolutePath(out)).iterator
    .filter(_.toNIO.toString.endsWith(".semanticdb"))
    .flatMap(file => TextDocuments.parseFrom(file.readAllBytes).documents).find(_.uri == name).get

  private def fromClassfile(a: Map[String, String], b: Map[String, String], uri: String) =
    document(compile(b, compile(a).toString), uri)
  private def inOneRun(a: Map[String, String], b: Map[String, String], uri: String) =
    document(compile(a ++ b), uri)

  private def section(doc: s.TextDocument, name: String): String = s.Print
    .document(Format.Detailed, doc).linesIterator.dropWhile(_ != name + ":").drop(1)
    .takeWhile(_.nonEmpty).mkString("\n")
  private def symbols(doc: s.TextDocument): String = section(doc, "Symbols")
  // the printer reads the signatures of symbols, which a collision can make cyclic
  private def occurrences(doc: s.TextDocument): String = section(doc.withSymbols(Nil), "Occurrences")

  private val Info =
    """symbol: "(local\d+)"\s+kind: \w+\s+(?:properties: \d+\s+)?display_name: "([^"]*)"""".r
  private val Ref = """symbol: "(local\d+)"""".r

  // locals that the document refers to, without SymbolInformation in Symbols
  private def dangling(doc: s.TextDocument): List[String] = {
    val saved = doc.symbols.map(_.symbol).toSet
    Ref.findAllMatchIn(doc.toProtoString).map(_.group(1)).filterNot(saved).toList.distinct
      .sortBy(_.stripPrefix("local").toInt)
  }

  // locals that the document gives more than one name
  private def collisions(doc: s.TextDocument): List[String] = Info.findAllMatchIn(doc.toProtoString)
    .map(m => m.group(1) -> m.group(2)).toList.distinct.groupBy(_._1).collect {
      case (sym, names) if names.lengthCompare(1) > 0 =>
        names.map(_._2).sorted.mkString(sym + ": ", ", ", "")
    }.toList.sorted

  private lazy val sameFile = document(compile(Map("C.scala" -> c)), "C.scala")
  private lazy val joint = inOneRun(Map("A.scala" -> a), Map("B.scala" -> b), "B.scala")
  private lazy val classfile = fromClassfile(Map("A.scala" -> a), Map("B.scala" -> b), "B.scala")

  test("#1591: A in the same file: symbols") {
    assertNoDiff(
      symbols(sameFile),
      """|c/Defs. => final object Defs extends AnyRef { +9 decls }
         |  AnyRef => scala/AnyRef#
         |c/Defs.C# => class C extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |c/Defs.C#`<init>`(). => primary ctor <init>()
         |c/Defs.F# => type F[T] = AnyRef { def get: T }
         |  T => c/Defs.F#[T]
         |  AnyRef => scala/AnyRef#
         |  get => local4
         |c/Defs.F#[T] => typeparam T
         |c/Defs.User# => type User = AnyRef { def name: String }
         |  AnyRef => scala/AnyRef#
         |  name => local0
         |  String => scala/Predef.String#
         |c/Defs.e. => val method e: List[List[_] forSome { type _ }]
         |  List => scala/package.List#
         |  _ => local8
         |c/Defs.lub. => val method lub: AnyRef { def z: Int }
         |  AnyRef => scala/AnyRef#
         |  z => local10
         |  Int => scala/Int#
         |c/Defs.m. => val method m: AnyRef { def poly[T](t: T): T }
         |  AnyRef => scala/AnyRef#
         |  poly => local5
         |  T => local7
         |  t => local6
         |c/Defs.r. => val method r: C { def y: Int }
         |  C => c/Defs.C#
         |  y => local3
         |  Int => scala/Int#
         |c/Defs.take(). => method take(p: AnyRef { def id: Int }): Int
         |  p => c/Defs.take().(p)
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |c/Defs.take().(p) => param p: AnyRef { def id: Int }
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |c/Defs.u. => val method u: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |c/Use. => final object Use extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |c/Use.age. => val method age: Int
         |  Int => scala/Int#
         |c/Use.f(). => method f(p: AnyRef { def id: Int }): Int
         |  p => c/Use.f().(p)
         |  AnyRef => scala/AnyRef#
         |  id => local13
         |  Int => scala/Int#
         |c/Use.f().(p) => param p: AnyRef { def id: Int }
         |  AnyRef => scala/AnyRef#
         |  id => local13
         |  Int => scala/Int#
         |c/Use.get. => val method get: Int
         |  Int => scala/Int#
         |c/Use.inferred. => val method inferred: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |c/Use.inferredDef(). => method inferredDef: C { def y: Int }
         |  C => c/Defs.C#
         |  y => local3
         |  Int => scala/Int#
         |c/Use.inferredE. => val method inferredE: List[List[_] forSome { type _ }]
         |  List => scala/package.List#
         |  _ => local8
         |c/Use.inferredF. => val method inferredF: F[Int]
         |  F => c/Defs.F#
         |  Int => scala/Int#
         |c/Use.inferredM. => val method inferredM: AnyRef { def poly[T](t: T): T }
         |  AnyRef => scala/AnyRef#
         |  poly => local5
         |  T => local7
         |  t => local6
         |c/Use.inferredR. => val method inferredR: C { def y: Int }
         |  C => c/Defs.C#
         |  y => local3
         |  Int => scala/Int#
         |c/Use.name. => val method name: String
         |  String => scala/Predef.String#
         |c/Use.poly. => val method poly: Int
         |  Int => scala/Int#
         |c/Use.takeFn. => val method takeFn: Function1[AnyRef { def id: Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |c/Use.user. => val method user: User
         |  User => c/Defs.User#
         |c/Use.viaParam. => val method viaParam: Option[Int]
         |  Option => scala/Option#
         |  Int => scala/Int#
         |c/Use.y. => val method y: Int
         |  Int => scala/Int#
         |c/Use.z. => val method z: Int
         |  Int => scala/Int#
         |local0 => abstract method name: String
         |  String => scala/Predef.String#
         |local1 => abstract method age: Int
         |  Int => scala/Int#
         |local2 => abstract method id: Int
         |  Int => scala/Int#
         |local3 => abstract method y: Int
         |  Int => scala/Int#
         |local4 => abstract method get: T
         |  T => c/Defs.F#[T]
         |local5 => abstract method poly[T](t: T): T
         |  T => local7
         |  t => local6
         |local6 => param t: T
         |  T => local7
         |local7 => typeparam T
         |local8 => synthetic abstract type _
         |local9 => final class $anon extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local10 => method z: Int
         |  Int => scala/Int#
         |local11 => final class $anon extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local12 => method z: Int
         |  Int => scala/Int#
         |local13 => abstract method id: Int
         |  Int => scala/Int#
         |local14 => param p: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("#1591: A in the same file: occurrences") {
    assertNoDiff(
      occurrences(sameFile),
      """|[0:8..0:9): c <= c/
         |[1:7..1:11): Defs => c/Defs.
         |[2:7..2:11): Defs <= c/Defs.
         |[3:7..3:11): User <= c/Defs.User#
         |[3:20..3:24): name <= local0
         |[3:26..3:32): String => scala/Predef.String#
         |[4:6..4:7): u <= c/Defs.u.
         |[4:15..4:18): age <= local1
         |[4:20..4:23): Int => scala/Int#
         |[4:28..4:31): ??? => scala/Predef.`???`().
         |[5:6..5:10): take <= c/Defs.take().
         |[5:11..5:12): p <= c/Defs.take().(p)
         |[5:20..5:22): id <= local2
         |[5:24..5:27): Int => scala/Int#
         |[5:32..5:35): Int => scala/Int#
         |[5:38..5:39): p => c/Defs.take().(p)
         |[5:40..5:42): id => local2
         |[6:8..6:9): C <= c/Defs.C#
         |[6:9..6:9):  <= c/Defs.C#`<init>`().
         |[7:6..7:7): r <= c/Defs.r.
         |[7:9..7:10): C => c/Defs.C#
         |[7:17..7:18): y <= local3
         |[7:20..7:23): Int => scala/Int#
         |[7:28..7:31): ??? => scala/Predef.`???`().
         |[8:7..8:8): F <= c/Defs.F#
         |[8:9..8:10): T <= c/Defs.F#[T]
         |[8:20..8:23): get <= local4
         |[8:25..8:26): T => c/Defs.F#[T]
         |[9:6..9:7): m <= c/Defs.m.
         |[9:15..9:19): poly <= local5
         |[9:20..9:21): T <= local7
         |[9:23..9:24): t <= local6
         |[9:26..9:27): T => local7
         |[9:30..9:31): T => local7
         |[9:36..9:39): ??? => scala/Predef.`???`().
         |[10:6..10:7): e <= c/Defs.e.
         |[10:9..10:13): List => scala/package.List#
         |[10:14..10:18): List => scala/package.List#
         |[10:19..10:20): _ <= local8
         |[10:25..10:28): ??? => scala/Predef.`???`().
         |[11:6..11:9): lub <= c/Defs.lub.
         |[11:26..11:26):  <= local9
         |[11:32..11:33): z <= local10
         |[11:49..11:49):  <= local11
         |[11:55..11:56): z <= local12
         |[13:7..13:10): Use <= c/Use.
         |[14:6..14:10): user <= c/Use.user.
         |[14:12..14:16): User => c/Defs.User#
         |[14:19..14:22): ??? => scala/Predef.`???`().
         |[15:6..15:10): name <= c/Use.name.
         |[15:13..15:17): user => c/Use.user.
         |[15:18..15:22): name => local0
         |[16:6..16:9): age <= c/Use.age.
         |[16:12..16:13): u => c/Defs.u.
         |[16:14..16:17): age => local1
         |[17:6..17:7): f <= c/Use.f().
         |[17:8..17:9): p <= c/Use.f().(p)
         |[17:17..17:19): id <= local13
         |[17:21..17:24): Int => scala/Int#
         |[17:30..17:31): p => c/Use.f().(p)
         |[17:32..17:34): id => local13
         |[18:6..18:14): viaParam <= c/Use.viaParam.
         |[18:17..18:23): Option => scala/Option.
         |[18:24..18:25): u => c/Defs.u.
         |[18:27..18:30): map => scala/Option#map().
         |[18:31..18:32): p <= local14
         |[18:36..18:37): p => local14
         |[18:38..18:41): age => local1
         |[19:6..19:7): y <= c/Use.y.
         |[19:10..19:11): r => c/Defs.r.
         |[19:12..19:13): y => local3
         |[20:6..20:9): get <= c/Use.get.
         |[20:13..20:16): ??? => scala/Predef.`???`().
         |[20:19..20:20): F => c/Defs.F#
         |[20:21..20:24): Int => scala/Int#
         |[20:27..20:30): get => local4
         |[21:6..21:10): poly <= c/Use.poly.
         |[21:13..21:14): m => c/Defs.m.
         |[21:15..21:19): poly => local5
         |[22:6..22:7): z <= c/Use.z.
         |[22:10..22:13): lub => c/Defs.lub.
         |[22:14..22:15): z => local10
         |[23:6..23:14): inferred <= c/Use.inferred.
         |[23:17..23:18): u => c/Defs.u.
         |[24:6..24:15): inferredR <= c/Use.inferredR.
         |[24:18..24:19): r => c/Defs.r.
         |[25:6..25:17): inferredDef <= c/Use.inferredDef().
         |[25:20..25:21): r => c/Defs.r.
         |[26:6..26:15): inferredF <= c/Use.inferredF.
         |[26:19..26:22): ??? => scala/Predef.`???`().
         |[26:25..26:26): F => c/Defs.F#
         |[26:27..26:30): Int => scala/Int#
         |[27:6..27:15): inferredM <= c/Use.inferredM.
         |[27:18..27:19): m => c/Defs.m.
         |[28:6..28:15): inferredE <= c/Use.inferredE.
         |[28:18..28:19): e => c/Defs.e.
         |[29:6..29:12): takeFn <= c/Use.takeFn.
         |[29:15..29:19): take => c/Defs.take().
         |""".stripMargin,
    )
  }

  test("#1591: A in another file of the run: symbols") {
    def printed = symbols(joint)
    assertNoDiff(
      printed,
      """|b/Use. => final object Use extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |b/Use.age. => val method age: Int
         |  Int => scala/Int#
         |b/Use.f(). => method f(p: AnyRef { def id: Int }): Int
         |  p => b/Use.f().(p)
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |b/Use.f().(p) => param p: AnyRef { def id: Int }
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |b/Use.get. => val method get: Int
         |  Int => scala/Int#
         |b/Use.inferred. => val method inferred: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |b/Use.inferredDef(). => method inferredDef: C { def y: Int }
         |  C => a/Defs.C#
         |  y => local4
         |  Int => scala/Int#
         |b/Use.inferredE. => val method inferredE: List[List[_] forSome { type _ }]
         |  List => scala/package.List#
         |  _ => local10
         |b/Use.inferredF. => val method inferredF: F[Int]
         |  F => a/Defs.F#
         |  Int => scala/Int#
         |b/Use.inferredM. => val method inferredM: AnyRef { def poly[T](t: T): T }
         |  AnyRef => scala/AnyRef#
         |  poly => local6
         |  T => local9
         |  t => local8
         |b/Use.inferredR. => val method inferredR: C { def y: Int }
         |  C => a/Defs.C#
         |  y => local4
         |  Int => scala/Int#
         |b/Use.name. => val method name: String
         |  String => scala/Predef.String#
         |b/Use.poly. => val method poly: Int
         |  Int => scala/Int#
         |b/Use.takeFn. => val method takeFn: Function1[AnyRef { def id: Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  id => local11
         |  Int => scala/Int#
         |b/Use.user. => val method user: User
         |  User => a/Defs.User#
         |b/Use.viaParam. => val method viaParam: Option[Int]
         |  Option => scala/Option#
         |  Int => scala/Int#
         |b/Use.y. => val method y: Int
         |  Int => scala/Int#
         |b/Use.z. => val method z: Int
         |  Int => scala/Int#
         |local0 => abstract method name: String
         |  String => scala/Predef.String#
         |local1 => abstract method age: Int
         |  Int => scala/Int#
         |local2 => abstract method id: Int
         |  Int => scala/Int#
         |local3 => param p: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |local4 => abstract method y: Int
         |  Int => scala/Int#
         |local5 => abstract method get: T
         |  T => a/Defs.F#[T]
         |local6 => abstract method poly[T](t: T): T
         |  T => local9
         |  t => local8
         |local7 => method z: Int
         |  Int => scala/Int#
         |local8 => param t: T
         |  T => local9
         |local9 => typeparam T
         |local11 => abstract method id: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("#1591: A in another file of the run: occurrences") {
    assertNoDiff(
      occurrences(joint),
      """|[0:8..0:9): b <= b/
         |[1:7..1:8): a => a/
         |[1:9..1:13): Defs => a/Defs.
         |[2:7..2:10): Use <= b/Use.
         |[3:6..3:10): user <= b/Use.user.
         |[3:12..3:16): User => a/Defs.User#
         |[3:19..3:22): ??? => scala/Predef.`???`().
         |[4:6..4:10): name <= b/Use.name.
         |[4:13..4:17): user => b/Use.user.
         |[4:18..4:22): name => local0
         |[5:6..5:9): age <= b/Use.age.
         |[5:12..5:13): u => a/Defs.u.
         |[5:14..5:17): age => local1
         |[6:6..6:7): f <= b/Use.f().
         |[6:8..6:9): p <= b/Use.f().(p)
         |[6:17..6:19): id <= local2
         |[6:21..6:24): Int => scala/Int#
         |[6:30..6:31): p => b/Use.f().(p)
         |[6:32..6:34): id => local2
         |[7:6..7:14): viaParam <= b/Use.viaParam.
         |[7:17..7:23): Option => scala/Option.
         |[7:24..7:25): u => a/Defs.u.
         |[7:27..7:30): map => scala/Option#map().
         |[7:31..7:32): p <= local3
         |[7:36..7:37): p => local3
         |[7:38..7:41): age => local1
         |[8:6..8:7): y <= b/Use.y.
         |[8:10..8:11): r => a/Defs.r.
         |[8:12..8:13): y => local4
         |[9:6..9:9): get <= b/Use.get.
         |[9:13..9:16): ??? => scala/Predef.`???`().
         |[9:19..9:20): F => a/Defs.F#
         |[9:21..9:24): Int => scala/Int#
         |[9:27..9:30): get => local5
         |[10:6..10:10): poly <= b/Use.poly.
         |[10:13..10:14): m => a/Defs.m.
         |[10:15..10:19): poly => local6
         |[11:6..11:7): z <= b/Use.z.
         |[11:10..11:13): lub => a/Defs.lub.
         |[11:14..11:15): z => local7
         |[12:6..12:14): inferred <= b/Use.inferred.
         |[12:17..12:18): u => a/Defs.u.
         |[13:6..13:15): inferredR <= b/Use.inferredR.
         |[13:18..13:19): r => a/Defs.r.
         |[14:6..14:17): inferredDef <= b/Use.inferredDef().
         |[14:20..14:21): r => a/Defs.r.
         |[15:6..15:15): inferredF <= b/Use.inferredF.
         |[15:19..15:22): ??? => scala/Predef.`???`().
         |[15:25..15:26): F => a/Defs.F#
         |[15:27..15:30): Int => scala/Int#
         |[16:6..16:15): inferredM <= b/Use.inferredM.
         |[16:18..16:19): m => a/Defs.m.
         |[17:6..17:15): inferredE <= b/Use.inferredE.
         |[17:18..17:19): e => a/Defs.e.
         |[18:6..18:12): takeFn <= b/Use.takeFn.
         |[18:15..18:19): take => a/Defs.take().
         |""".stripMargin,
    )
  }

  test("#1591: A from a classfile: symbols") {
    assertNoDiff(
      symbols(classfile),
      """|b/Use. => final object Use extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |b/Use.age. => val method age: Int
         |  Int => scala/Int#
         |b/Use.f(). => method f(p: AnyRef { def id: Int }): Int
         |  p => b/Use.f().(p)
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |b/Use.f().(p) => param p: AnyRef { def id: Int }
         |  AnyRef => scala/AnyRef#
         |  id => local2
         |  Int => scala/Int#
         |b/Use.get. => val method get: Int
         |  Int => scala/Int#
         |b/Use.inferred. => val method inferred: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |b/Use.inferredDef(). => method inferredDef: C { def y: Int }
         |  C => a/Defs.C#
         |  y => local9
         |  Int => scala/Int#
         |b/Use.inferredE. => val method inferredE: List[List[_] forSome { type _ }]
         |  List => scala/package.List#
         |  _ => local12
         |b/Use.inferredF. => val method inferredF: F[Int]
         |  F => a/Defs.F#
         |  Int => scala/Int#
         |b/Use.inferredM. => val method inferredM: AnyRef { def poly[T](t: T): T }
         |  AnyRef => scala/AnyRef#
         |  poly => local6
         |  T => local11
         |  t => local10
         |b/Use.inferredR. => val method inferredR: C { def y: Int }
         |  C => a/Defs.C#
         |  y => local8
         |  Int => scala/Int#
         |b/Use.name. => val method name: String
         |  String => scala/Predef.String#
         |b/Use.poly. => val method poly: Int
         |  Int => scala/Int#
         |b/Use.takeFn. => val method takeFn: Function1[AnyRef { def id: Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  id => local13
         |  Int => scala/Int#
         |b/Use.user. => val method user: User
         |  User => a/Defs.User#
         |b/Use.viaParam. => val method viaParam: Option[Int]
         |  Option => scala/Option#
         |  Int => scala/Int#
         |b/Use.y. => val method y: Int
         |  Int => scala/Int#
         |b/Use.z. => val method z: Int
         |  Int => scala/Int#
         |local0 => abstract method name: String
         |  String => scala/Predef.String#
         |local1 => abstract method age: Int
         |  Int => scala/Int#
         |local2 => abstract method id: Int
         |  Int => scala/Int#
         |local3 => param p: AnyRef { def age: Int }
         |  AnyRef => scala/AnyRef#
         |  age => local1
         |  Int => scala/Int#
         |local4 => abstract method y: Int
         |  Int => scala/Int#
         |local5 => abstract method get: Int
         |  Int => scala/Int#
         |local6 => abstract method poly[T](t: T): T
         |  T => local11
         |  t => local10
         |local7 => method z: Int
         |  Int => scala/Int#
         |local8 => abstract method y: Int
         |  Int => scala/Int#
         |local9 => abstract method y: Int
         |  Int => scala/Int#
         |local10 => param t: T
         |  T => local11
         |local11 => typeparam T
         |local13 => abstract method id: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("#1591: A from a classfile: occurrences") {
    assertNoDiff(
      occurrences(classfile),
      """|[0:8..0:9): b <= b/
         |[1:7..1:8): a => a/
         |[1:9..1:13): Defs => a/Defs.
         |[2:7..2:10): Use <= b/Use.
         |[3:6..3:10): user <= b/Use.user.
         |[3:12..3:16): User => a/Defs.User#
         |[3:19..3:22): ??? => scala/Predef.`???`().
         |[4:6..4:10): name <= b/Use.name.
         |[4:13..4:17): user => b/Use.user.
         |[4:18..4:22): name => local0
         |[5:6..5:9): age <= b/Use.age.
         |[5:12..5:13): u => a/Defs.u.
         |[5:14..5:17): age => local1
         |[6:6..6:7): f <= b/Use.f().
         |[6:8..6:9): p <= b/Use.f().(p)
         |[6:17..6:19): id <= local2
         |[6:21..6:24): Int => scala/Int#
         |[6:30..6:31): p => b/Use.f().(p)
         |[6:32..6:34): id => local2
         |[7:6..7:14): viaParam <= b/Use.viaParam.
         |[7:17..7:23): Option => scala/Option.
         |[7:24..7:25): u => a/Defs.u.
         |[7:27..7:30): map => scala/Option#map().
         |[7:31..7:32): p <= local3
         |[7:36..7:37): p => local3
         |[7:38..7:41): age => local1
         |[8:6..8:7): y <= b/Use.y.
         |[8:10..8:11): r => a/Defs.r.
         |[8:12..8:13): y => local4
         |[9:6..9:9): get <= b/Use.get.
         |[9:13..9:16): ??? => scala/Predef.`???`().
         |[9:19..9:20): F => a/Defs.F#
         |[9:21..9:24): Int => scala/Int#
         |[9:27..9:30): get => local5
         |[10:6..10:10): poly <= b/Use.poly.
         |[10:13..10:14): m => a/Defs.m.
         |[10:15..10:19): poly => local6
         |[11:6..11:7): z <= b/Use.z.
         |[11:10..11:13): lub => a/Defs.lub.
         |[11:14..11:15): z => local7
         |[12:6..12:14): inferred <= b/Use.inferred.
         |[12:17..12:18): u => a/Defs.u.
         |[13:6..13:15): inferredR <= b/Use.inferredR.
         |[13:18..13:19): r => a/Defs.r.
         |[14:6..14:17): inferredDef <= b/Use.inferredDef().
         |[14:20..14:21): r => a/Defs.r.
         |[15:6..15:15): inferredF <= b/Use.inferredF.
         |[15:19..15:22): ??? => scala/Predef.`???`().
         |[15:25..15:26): F => a/Defs.F#
         |[15:27..15:30): Int => scala/Int#
         |[16:6..16:15): inferredM <= b/Use.inferredM.
         |[16:18..16:19): m => a/Defs.m.
         |[17:6..17:15): inferredE <= b/Use.inferredE.
         |[17:18..17:19): e => a/Defs.e.
         |[18:6..18:12): takeFn <= b/Use.takeFn.
         |[18:15..18:19): take => a/Defs.take().
         |""".stripMargin,
    )
  }

  test("#1591: A in another file of the run, and A from a classfile")(
    assertNotEquals(joint, classfile),
  )

  test("#1591: A in another file of the run, with a local of B at the offset of a member of A") {
    val doc = inOneRun(Map("A.scala" -> offsetA), Map("B.scala" -> offsetB), "B.scala")
    assertNoDiff(
      symbols(doc),
      """|b/Use. => final object Use extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/Use.inferredR. => val method inferredR: C { def y: Int }
         |  C => a/Defs.C#
         |  y => local1
         |  Int => scala/Int#
         |b/Use.sameOffsetAsY(). => method sameOffsetAsY: Int
         |  Int => scala/Int#
         |local0 => val local y: Int
         |  Int => scala/Int#
         |local1 => abstract method y: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("#1591: local names") {
    assertEquals(collisions(sameFile), Nil)
    assertEquals(collisions(joint), Nil)
    assertEquals(collisions(classfile), Nil)
  }

  test("#1591: local references") {
    assertEquals(dangling(sameFile), Nil)
    assertEquals(dangling(joint), List("local10"))
    assertEquals(dangling(classfile), List("local12"))
  }

  test("#1591: two refinements with one member name under one owner, from a classfile: symbols") {
    val doc =
      fromClassfile(Map("Twins.scala" -> twinsA), Map("UseTwins.scala" -> twinsB), "UseTwins.scala")
    assertNoDiff(
      symbols(doc),
      """|b/UseTwins. => final object UseTwins extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseTwins.pair. => val method pair: Tuple2[AnyRef { def x: Int }, AnyRef { def x: Int }]
         |  Tuple2 => scala/Tuple2#
         |  AnyRef => scala/AnyRef#
         |  x => local2
         |  Int => scala/Int#
         |  x => local3
         |b/UseTwins.two. => val method two: Function2[AnyRef { def x: Int }, AnyRef { def x: Int }, Int]
         |  Function2 => scala/Function2#
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |  x => local1
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: Int
         |  Int => scala/Int#
         |local2 => abstract method x: Int
         |  Int => scala/Int#
         |local3 => abstract method x: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("#1591: two refinements with one member name under one owner, in another file of the run: symbols") {
    val doc =
      inOneRun(Map("Twins.scala" -> twinsA), Map("UseTwins.scala" -> twinsB), "UseTwins.scala")
    assertNoDiff(
      symbols(doc),
      """|b/UseTwins. => final object UseTwins extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseTwins.pair. => val method pair: Tuple2[AnyRef { def x: Int }, AnyRef { def x: Int }]
         |  Tuple2 => scala/Tuple2#
         |  AnyRef => scala/AnyRef#
         |  x => local2
         |  Int => scala/Int#
         |  x => local3
         |b/UseTwins.two. => val method two: Function2[AnyRef { def x: Int }, AnyRef { def x: Int }, Int]
         |  Function2 => scala/Function2#
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |  x => local1
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: Int
         |  Int => scala/Int#
         |local2 => abstract method x: Int
         |  Int => scala/Int#
         |local3 => abstract method x: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  private lazy val mixedClassfile =
    fromClassfile(Map("Mixed.scala" -> mixedA), Map("UseMixed.scala" -> mixedB), "UseMixed.scala")

  test("#1591: refinements with one member name under one owner, from a classfile: symbols") {
    assertNoDiff(
      symbols(mixedClassfile),
      """|b/UseMixed. => final object UseMixed extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseMixed.copies. => val method copies: Tuple2[C { def x: Int }, C { def x: Int }]
         |  Tuple2 => scala/Tuple2#
         |  C => a/Mixed.C#
         |  x => local2
         |  Int => scala/Int#
         |  x => local3
         |b/UseMixed.types. => val method types: Tuple2[AnyRef { def x: Int }, AnyRef { def x: String }]
         |  Tuple2 => scala/Tuple2#
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |  x => local1
         |  String => scala/Predef.String#
         |b/UseMixed.x. => val method x: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: String
         |  String => scala/Predef.String#
         |local2 => abstract method x: Int
         |  Int => scala/Int#
         |local3 => abstract method x: Int
         |  Int => scala/Int#
         |local4 => abstract method x: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("#1591: refinements with one member name under one owner, from a classfile: occurrences") {
    assertNoDiff(
      occurrences(mixedClassfile),
      """|[0:8..0:9): b <= b/
         |[1:7..1:15): UseMixed <= b/UseMixed.
         |[2:6..2:11): types <= b/UseMixed.types.
         |[2:14..2:15): a => a/
         |[2:16..2:21): Mixed => a/Mixed.
         |[2:22..2:27): types => a/Mixed.types.
         |[3:6..3:12): copies <= b/UseMixed.copies.
         |[3:15..3:16): a => a/
         |[3:17..3:22): Mixed => a/Mixed.
         |[3:23..3:29): copies => a/Mixed.copies.
         |[4:6..4:7): x <= b/UseMixed.x.
         |[4:10..4:11): a => a/
         |[4:12..4:17): Mixed => a/Mixed.
         |[4:18..4:24): copies => a/Mixed.copies.
         |[4:25..4:27): _2 => scala/Tuple2#_2.
         |[4:28..4:29): x => local4
         |""".stripMargin,
    )
  }

  test(
    "#1591: refinements with one member name under one owner, in another file of the run: symbols",
  ) {
    val doc =
      inOneRun(Map("Mixed.scala" -> mixedA), Map("UseMixed.scala" -> mixedB), "UseMixed.scala")
    assertNoDiff(
      symbols(doc),
      """|b/UseMixed. => final object UseMixed extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseMixed.copies. => val method copies: Tuple2[C { def x: Int }, C { def x: Int }]
         |  Tuple2 => scala/Tuple2#
         |  C => a/Mixed.C#
         |  x => local2
         |  Int => scala/Int#
         |  x => local3
         |b/UseMixed.types. => val method types: Tuple2[AnyRef { def x: Int }, AnyRef { def x: String }]
         |  Tuple2 => scala/Tuple2#
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |  x => local1
         |  String => scala/Predef.String#
         |b/UseMixed.x. => val method x: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: String
         |  String => scala/Predef.String#
         |local2 => abstract method x: Int
         |  Int => scala/Int#
         |local3 => abstract method x: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  private def summary(doc: s.TextDocument): String = symbols(doc) + "\n----\n" + occurrences(doc) +
    "\n----\ncollisions " + collisions(doc) + "\ndangling " + dangling(doc)

  private val overloadsA = Map(
    "Ov.scala" ->
      """|package a
         |object Ov {
         |  val o: { def f(x: Int): Int; def f(x: String): Int } = ???
         |}
         |""".stripMargin,
  )

  private val overloadsB = Map(
    "UseOv.scala" ->
      """|package b
         |object UseOv {
         |  val i = a.Ov.o.f(x = 1)
         |  val s = a.Ov.o.f(x = "")
         |}
         |""".stripMargin,
  )

  test("#1591: overloads in one refinement, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(overloadsA, overloadsB, "UseOv.scala")),
      """|b/UseOv. => final object UseOv extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseOv.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseOv.s. => val method s: Int
         |  Int => scala/Int#
         |local0 => param x: Int
         |  Int => scala/Int#
         |local1 => abstract method f(x: Int): Int
         |  x => local0
         |  Int => scala/Int#
         |local2 => param x: String
         |  String => scala/Predef.String#
         |local3 => abstract method f(x: String): Int
         |  x => local2
         |  String => scala/Predef.String#
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): UseOv <= b/UseOv.
         |[2:6..2:7): i <= b/UseOv.i.
         |[2:10..2:11): a => a/
         |[2:12..2:14): Ov => a/Ov.
         |[2:15..2:16): o => a/Ov.o.
         |[2:17..2:18): f => local1
         |[2:19..2:20): x => local0
         |[3:6..3:7): s <= b/UseOv.s.
         |[3:10..3:11): a => a/
         |[3:12..3:14): Ov => a/Ov.
         |[3:15..3:16): o => a/Ov.o.
         |[3:17..3:18): f => local3
         |[3:19..3:20): x => local2
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: overloads in one refinement, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(overloadsA, overloadsB, "UseOv.scala")),
      """|b/UseOv. => final object UseOv extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseOv.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseOv.s. => val method s: Int
         |  Int => scala/Int#
         |local0 => param x: Int
         |  Int => scala/Int#
         |local1 => abstract method f(x: Int): Int
         |  x => local0
         |  Int => scala/Int#
         |local2 => param x: String
         |  String => scala/Predef.String#
         |local3 => abstract method f(x: String): Int
         |  x => local2
         |  String => scala/Predef.String#
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): UseOv <= b/UseOv.
         |[2:6..2:7): i <= b/UseOv.i.
         |[2:10..2:11): a => a/
         |[2:12..2:14): Ov => a/Ov.
         |[2:15..2:16): o => a/Ov.o.
         |[2:17..2:18): f => local1
         |[2:19..2:20): x => local0
         |[3:6..3:7): s <= b/UseOv.s.
         |[3:10..3:11): a => a/
         |[3:12..3:14): Ov => a/Ov.
         |[3:15..3:16): o => a/Ov.o.
         |[3:17..3:18): f => local3
         |[3:19..3:20): x => local2
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // `early` is complete only if another file of the run uses it
  private val batchA = Map(
    "M.scala" ->
      """|package a
         |object M {
         |  class C
         |  def early: { def x: Int } = ???
         |  val t: { def x: Int } = ???
         |  val v: C { def x: Int } = ???
         |}
         |""".stripMargin,
  )

  private val batchB = Map(
    "UseM.scala" ->
      """|package b
         |object UseM {
         |  val t = a.M.t.x
         |  val v = a.M.v.x
         |}
         |""".stripMargin,
  )

  private val batchC = Map(
    "Force.scala" ->
      """|package c
         |object Force {
         |  val e = a.M.early
         |}
         |""".stripMargin,
  )

  test("#1591: refinements with one member name, from a classfile, alone") {
    assertNoDiff(
      summary(fromClassfile(batchA, batchB, "UseM.scala")),
      """|b/UseM. => final object UseM extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseM.t. => val method t: Int
         |  Int => scala/Int#
         |b/UseM.v. => val method v: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseM <= b/UseM.
         |[2:6..2:7): t <= b/UseM.t.
         |[2:10..2:11): a => a/
         |[2:12..2:13): M => a/M.
         |[2:14..2:15): t => a/M.t.
         |[2:16..2:17): x => local0
         |[3:6..3:7): v <= b/UseM.v.
         |[3:10..3:11): a => a/
         |[3:12..3:13): M => a/M.
         |[3:14..3:15): v => a/M.v.
         |[3:16..3:17): x => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: refinements with one member name, from a classfile, with a file that uses `early`") {
    assertNoDiff(
      summary(fromClassfile(batchA, batchB ++ batchC, "UseM.scala")),
      """|b/UseM. => final object UseM extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseM.t. => val method t: Int
         |  Int => scala/Int#
         |b/UseM.v. => val method v: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseM <= b/UseM.
         |[2:6..2:7): t <= b/UseM.t.
         |[2:10..2:11): a => a/
         |[2:12..2:13): M => a/M.
         |[2:14..2:15): t => a/M.t.
         |[2:16..2:17): x => local0
         |[3:6..3:7): v <= b/UseM.v.
         |[3:10..3:11): a => a/
         |[3:12..3:13): M => a/M.
         |[3:14..3:15): v => a/M.v.
         |[3:16..3:17): x => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  private val shapesA = Map {
    "N.scala" ->
      """|package a
         |object N {
         |  val n: { def inner: { def x: Int } } = ???
         |  val g: { def x: Int } = ???
         |  type Al = { def x: Int; def y: Int }
         |  type Bd <: { def x: Int }
         |  val bd: Bd = ???
         |  def meth(p: { def m(q: Int): Int }): Int = p.m(q = 1)
         |  type F[T] = { def get: T }
         |  class Nest { val r: { def x: Int } = ??? }
         |  trait S { self: { def x: Int } => }
         |  class Base[T] { def get: T = ??? }
         |  class P extends Base[{ def x: Int }]
         |}
         |""".stripMargin
  }

  private val shapesB = Map {
    "UseN.scala" ->
      """|package b
         |import a.N._
         |object UseN {
         |  val nx = n.inner.x
         |  val gx = g.x
         |  val alx = (??? : Al).x
         |  val aly = (??? : Al).y
         |  val bdx = bd.x
         |  val fi = (??? : F[Int]).get
         |  val fs = (??? : F[String]).get
         |  val nest = new Nest().r.x
         |  val px = new P().get.x
         |  def mq(p: { def m(q: Int): Int }) = p.m(q = 2)
         |  val takeM = meth _
         |}
         |""".stripMargin
  }

  test("#1591: refinements of every shape, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(shapesA, shapesB, "UseN.scala")),
      """|b/UseN. => final object UseN extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseN.alx. => val method alx: Int
         |  Int => scala/Int#
         |b/UseN.aly. => val method aly: Int
         |  Int => scala/Int#
         |b/UseN.bdx. => val method bdx: Int
         |  Int => scala/Int#
         |b/UseN.fi. => val method fi: Int
         |  Int => scala/Int#
         |b/UseN.fs. => val method fs: String
         |  String => scala/Predef.String#
         |b/UseN.gx. => val method gx: Int
         |  Int => scala/Int#
         |b/UseN.mq(). => method mq(p: AnyRef { def m(q: Int): Int }): Int
         |  p => b/UseN.mq().(p)
         |  AnyRef => scala/AnyRef#
         |  m => local10
         |  q => local11
         |  Int => scala/Int#
         |b/UseN.mq().(p) => param p: AnyRef { def m(q: Int): Int }
         |  AnyRef => scala/AnyRef#
         |  m => local10
         |  q => local11
         |  Int => scala/Int#
         |b/UseN.nest. => val method nest: Int
         |  Int => scala/Int#
         |b/UseN.nx. => val method nx: Int
         |  Int => scala/Int#
         |b/UseN.px. => val method px: Int
         |  Int => scala/Int#
         |b/UseN.takeM. => val method takeM: Function1[AnyRef { def m(q: Int): Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  m => local12
         |  q => local13
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method inner: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |local2 => abstract method x: Int
         |  Int => scala/Int#
         |local3 => abstract method x: Int
         |  Int => scala/Int#
         |local4 => abstract method y: Int
         |  Int => scala/Int#
         |local5 => abstract method x: Int
         |  Int => scala/Int#
         |local6 => abstract method get: Int
         |  Int => scala/Int#
         |local7 => abstract method get: String
         |  String => scala/Predef.String#
         |local8 => abstract method x: Int
         |  Int => scala/Int#
         |local9 => abstract method x: Int
         |  Int => scala/Int#
         |local10 => abstract method m(q: Int): Int
         |  q => local11
         |  Int => scala/Int#
         |local11 => param q: Int
         |  Int => scala/Int#
         |local12 => abstract method m(q: Int): Int
         |  q => local13
         |  Int => scala/Int#
         |local13 => param q: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:8): a => a/
         |[1:9..1:10): N => a/N.
         |[2:7..2:11): UseN <= b/UseN.
         |[3:6..3:8): nx <= b/UseN.nx.
         |[3:11..3:12): n => a/N.n.
         |[3:13..3:18): inner => local1
         |[3:19..3:20): x => local0
         |[4:6..4:8): gx <= b/UseN.gx.
         |[4:11..4:12): g => a/N.g.
         |[4:13..4:14): x => local2
         |[5:6..5:9): alx <= b/UseN.alx.
         |[5:13..5:16): ??? => scala/Predef.`???`().
         |[5:19..5:21): Al => a/N.Al#
         |[5:23..5:24): x => local3
         |[6:6..6:9): aly <= b/UseN.aly.
         |[6:13..6:16): ??? => scala/Predef.`???`().
         |[6:19..6:21): Al => a/N.Al#
         |[6:23..6:24): y => local4
         |[7:6..7:9): bdx <= b/UseN.bdx.
         |[7:12..7:14): bd => a/N.bd.
         |[7:15..7:16): x => local5
         |[8:6..8:8): fi <= b/UseN.fi.
         |[8:12..8:15): ??? => scala/Predef.`???`().
         |[8:18..8:19): F => a/N.F#
         |[8:20..8:23): Int => scala/Int#
         |[8:26..8:29): get => local6
         |[9:6..9:8): fs <= b/UseN.fs.
         |[9:12..9:15): ??? => scala/Predef.`???`().
         |[9:18..9:19): F => a/N.F#
         |[9:20..9:26): String => scala/Predef.String#
         |[9:29..9:32): get => local7
         |[10:6..10:10): nest <= b/UseN.nest.
         |[10:17..10:21): Nest => a/N.Nest#
         |[10:21..10:21):  => a/N.Nest#`<init>`().
         |[10:24..10:25): r => a/N.Nest#r.
         |[10:26..10:27): x => local8
         |[11:6..11:8): px <= b/UseN.px.
         |[11:15..11:16): P => a/N.P#
         |[11:16..11:16):  => a/N.P#`<init>`().
         |[11:19..11:22): get => a/N.Base#get().
         |[11:23..11:24): x => local9
         |[12:6..12:8): mq <= b/UseN.mq().
         |[12:9..12:10): p <= b/UseN.mq().(p)
         |[12:18..12:19): m <= local10
         |[12:20..12:21): q <= local11
         |[12:23..12:26): Int => scala/Int#
         |[12:29..12:32): Int => scala/Int#
         |[12:38..12:39): p => b/UseN.mq().(p)
         |[12:40..12:41): m => local10
         |[12:42..12:43): q => local11
         |[13:6..13:11): takeM <= b/UseN.takeM.
         |[13:14..13:18): meth => a/N.meth().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: refinements of every shape, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(shapesA, shapesB, "UseN.scala")),
      """|b/UseN. => final object UseN extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseN.alx. => val method alx: Int
         |  Int => scala/Int#
         |b/UseN.aly. => val method aly: Int
         |  Int => scala/Int#
         |b/UseN.bdx. => val method bdx: Int
         |  Int => scala/Int#
         |b/UseN.fi. => val method fi: Int
         |  Int => scala/Int#
         |b/UseN.fs. => val method fs: String
         |  String => scala/Predef.String#
         |b/UseN.gx. => val method gx: Int
         |  Int => scala/Int#
         |b/UseN.mq(). => method mq(p: AnyRef { def m(q: Int): Int }): Int
         |  p => b/UseN.mq().(p)
         |  AnyRef => scala/AnyRef#
         |  m => local9
         |  q => local10
         |  Int => scala/Int#
         |b/UseN.mq().(p) => param p: AnyRef { def m(q: Int): Int }
         |  AnyRef => scala/AnyRef#
         |  m => local9
         |  q => local10
         |  Int => scala/Int#
         |b/UseN.nest. => val method nest: Int
         |  Int => scala/Int#
         |b/UseN.nx. => val method nx: Int
         |  Int => scala/Int#
         |b/UseN.px. => val method px: Int
         |  Int => scala/Int#
         |b/UseN.takeM. => val method takeM: Function1[AnyRef { def m(q: Int): Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  m => local11
         |  q => local12
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method inner: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |local2 => abstract method x: Int
         |  Int => scala/Int#
         |local3 => abstract method x: Int
         |  Int => scala/Int#
         |local4 => abstract method y: Int
         |  Int => scala/Int#
         |local5 => abstract method x: Int
         |  Int => scala/Int#
         |local6 => abstract method get: T
         |  T => a/N.F#[T]
         |local7 => abstract method x: Int
         |  Int => scala/Int#
         |local8 => abstract method x: Int
         |  Int => scala/Int#
         |local9 => abstract method m(q: Int): Int
         |  q => local10
         |  Int => scala/Int#
         |local10 => param q: Int
         |  Int => scala/Int#
         |local11 => abstract method m(q: Int): Int
         |  q => local12
         |  Int => scala/Int#
         |local12 => param q: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:8): a => a/
         |[1:9..1:10): N => a/N.
         |[2:7..2:11): UseN <= b/UseN.
         |[3:6..3:8): nx <= b/UseN.nx.
         |[3:11..3:12): n => a/N.n.
         |[3:13..3:18): inner => local1
         |[3:19..3:20): x => local0
         |[4:6..4:8): gx <= b/UseN.gx.
         |[4:11..4:12): g => a/N.g.
         |[4:13..4:14): x => local2
         |[5:6..5:9): alx <= b/UseN.alx.
         |[5:13..5:16): ??? => scala/Predef.`???`().
         |[5:19..5:21): Al => a/N.Al#
         |[5:23..5:24): x => local3
         |[6:6..6:9): aly <= b/UseN.aly.
         |[6:13..6:16): ??? => scala/Predef.`???`().
         |[6:19..6:21): Al => a/N.Al#
         |[6:23..6:24): y => local4
         |[7:6..7:9): bdx <= b/UseN.bdx.
         |[7:12..7:14): bd => a/N.bd.
         |[7:15..7:16): x => local5
         |[8:6..8:8): fi <= b/UseN.fi.
         |[8:12..8:15): ??? => scala/Predef.`???`().
         |[8:18..8:19): F => a/N.F#
         |[8:20..8:23): Int => scala/Int#
         |[8:26..8:29): get => local6
         |[9:6..9:8): fs <= b/UseN.fs.
         |[9:12..9:15): ??? => scala/Predef.`???`().
         |[9:18..9:19): F => a/N.F#
         |[9:20..9:26): String => scala/Predef.String#
         |[9:29..9:32): get => local6
         |[10:6..10:10): nest <= b/UseN.nest.
         |[10:17..10:21): Nest => a/N.Nest#
         |[10:21..10:21):  => a/N.Nest#`<init>`().
         |[10:24..10:25): r => a/N.Nest#r.
         |[10:26..10:27): x => local7
         |[11:6..11:8): px <= b/UseN.px.
         |[11:15..11:16): P => a/N.P#
         |[11:16..11:16):  => a/N.P#`<init>`().
         |[11:19..11:22): get => a/N.Base#get().
         |[11:23..11:24): x => local8
         |[12:6..12:8): mq <= b/UseN.mq().
         |[12:9..12:10): p <= b/UseN.mq().(p)
         |[12:18..12:19): m <= local9
         |[12:20..12:21): q <= local10
         |[12:23..12:26): Int => scala/Int#
         |[12:29..12:32): Int => scala/Int#
         |[12:38..12:39): p => b/UseN.mq().(p)
         |[12:40..12:41): m => local9
         |[12:42..12:43): q => local10
         |[13:6..13:11): takeM <= b/UseN.takeM.
         |[13:14..13:18): meth => a/N.meth().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // the parent of a nested object holds a copy-prone refinement of the outer object
  private val nestedObjectA = Map(
    "Q.scala" ->
      """|package a
         |object Q {
         |  class C
         |  class Base[T] { def get: T = ??? }
         |  object Inner extends Base[C { def x: Int }]
         |  val g: C { def x: Int } = ???
         |}
         |""".stripMargin,
  )

  private val nestedObjectB = Map(
    "UseQ.scala" ->
      """|package b
         |object UseQ {
         |  val ix = a.Q.Inner.get.x
         |  val gx = a.Q.g.x
         |}
         |""".stripMargin,
  )

  test("#1591: a refinement in the parent of a nested object, from a classfile") {
    val doc = fromClassfile(nestedObjectA, nestedObjectB, "UseQ.scala")
    assertNoDiff(
      occurrences(doc).linesIterator.filter(_.contains(" x ")).mkString("\n"),
      """|[2:25..2:26): x => local0
         |[3:17..3:18): x => local1
         |""".stripMargin,
    )
  }

  test("#1591: two documents of one run") {
    val out =
      compile(Map("A.scala" -> a, "B.scala" -> b, "B2.scala" -> b.replace("package b", "package b2")))
    def symbolsOf(uri: String) = symbols(document(out, uri)).replace("b2/", "b/")
    assertEquals(symbolsOf("B2.scala"), symbolsOf("B.scala"))
  }

  private val selfA = Map(
    "S.scala" ->
      """|package a
         |trait S { self: { def x: Int } =>
         |  def useX = x
         |}
         |""".stripMargin,
  )

  private val selfB = Map(
    "T.scala" ->
      """|package b
         |trait T extends a.S { self: { def x: Int } =>
         |  def useY = x + useX
         |}
         |""".stripMargin,
  )

  test("#1591: a self type selected through, in the same file") {
    assertNoDiff(
      summary(document(compile(selfA), "S.scala")),
      """|a/S# => trait S extends AnyRef { self: AnyRef { def x: Int } => +2 decls }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |a/S#`<init>`(). => primary ctor <init>()
         |a/S#useX(). => method useX: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => selfparam self: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |----
         |[0:8..0:9): a <= a/
         |[1:6..1:7): S <= a/S#
         |[1:8..1:8):  <= a/S#`<init>`().
         |[1:10..1:14): self <= local1
         |[1:22..1:23): x <= local0
         |[1:25..1:28): Int => scala/Int#
         |[2:6..2:10): useX <= a/S#useX().
         |[2:13..2:14): x => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a self type selected through, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(selfA, selfB, "T.scala")),
      """|b/T# => trait T extends AnyRef with S { self: AnyRef { def x: Int } => +2 decls }
         |  AnyRef => scala/AnyRef#
         |  S => a/S#
         |  x => local0
         |  Int => scala/Int#
         |b/T#`<init>`(). => primary ctor <init>()
         |b/T#useY(). => method useY: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => selfparam self: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:6..1:7): T <= b/T#
         |[1:8..1:8):  <= b/T#`<init>`().
         |[1:16..1:17): a => a/
         |[1:18..1:19): S => a/S#
         |[1:22..1:26): self <= local1
         |[1:34..1:35): x <= local0
         |[1:37..1:40): Int => scala/Int#
         |[2:6..2:10): useY <= b/T#useY().
         |[2:13..2:14): x => local0
         |[2:15..2:16): + => scala/Int#`+`(+4).
         |[2:17..2:21): useX => a/S#useX().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a self type selected through, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(selfA, selfB, "T.scala")),
      """|b/T# => trait T extends AnyRef with S { self: AnyRef { def x: Int } => +2 decls }
         |  AnyRef => scala/AnyRef#
         |  S => a/S#
         |  x => local0
         |  Int => scala/Int#
         |b/T#`<init>`(). => primary ctor <init>()
         |b/T#useY(). => method useY: Int
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => selfparam self: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:6..1:7): T <= b/T#
         |[1:8..1:8):  <= b/T#`<init>`().
         |[1:16..1:17): a => a/
         |[1:18..1:19): S => a/S#
         |[1:22..1:26): self <= local1
         |[1:34..1:35): x <= local0
         |[1:37..1:40): Int => scala/Int#
         |[2:6..2:10): useY <= b/T#useY().
         |[2:13..2:14): x => local0
         |[2:15..2:16): + => scala/Int#`+`(+4).
         |[2:17..2:21): useX => a/S#useX().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  private val boxA = Map(
    "Box.scala" ->
      """|package a
         |class Box[T] { val v: { def get: T } = ??? }
         |""".stripMargin,
  )

  private val boxB = Map(
    "UseBox.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object UseBox {
         |  val i = new a.Box[Int].v.get
         |  val s = new a.Box[String].v.get
         |}
         |""".stripMargin,
  )

  test("#1591: a refinement in a generic class at two type arguments, in the same file") {
    val code = boxA("Box.scala") + boxB("UseBox.scala").replace("package b\n", "")
    assertNoDiff(
      summary(document(compile(Map("Box.scala" -> code)), "Box.scala")),
      """|a/Box# => class Box[T] extends AnyRef { +2 decls }
         |  T => a/Box#[T]
         |  AnyRef => scala/AnyRef#
         |a/Box#[T] => typeparam T
         |a/Box#`<init>`(). => primary ctor <init>()
         |a/Box#v. => val method v: AnyRef { def get: T }
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |  T => a/Box#[T]
         |a/UseBox. => final object UseBox extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |a/UseBox.i. => val method i: Int
         |  Int => scala/Int#
         |a/UseBox.s. => val method s: String
         |  String => scala/Predef.String#
         |local0 => abstract method get: T
         |  T => a/Box#[T]
         |----
         |[0:8..0:9): a <= a/
         |[1:6..1:9): Box <= a/Box#
         |[1:10..1:11): T <= a/Box#[T]
         |[1:13..1:13):  <= a/Box#`<init>`().
         |[1:19..1:20): v <= a/Box#v.
         |[1:28..1:31): get <= local0
         |[1:33..1:34): T => a/Box#[T]
         |[1:39..1:42): ??? => scala/Predef.`???`().
         |[2:7..2:12): scala => scala/
         |[2:13..2:21): language => scala/language.
         |[2:22..2:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[3:7..3:13): UseBox <= a/UseBox.
         |[4:6..4:7): i <= a/UseBox.i.
         |[4:14..4:15): a => a/
         |[4:16..4:19): Box => a/Box#
         |[4:20..4:23): Int => scala/Int#
         |[4:24..4:24):  => a/Box#`<init>`().
         |[4:25..4:26): v => a/Box#v.
         |[4:27..4:30): get => local0
         |[5:6..5:7): s <= a/UseBox.s.
         |[5:14..5:15): a => a/
         |[5:16..5:19): Box => a/Box#
         |[5:20..5:26): String => scala/Predef.String#
         |[5:27..5:27):  => a/Box#`<init>`().
         |[5:28..5:29): v => a/Box#v.
         |[5:30..5:33): get => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a refinement in a generic class at two type arguments, in the same file, after a use") {
    val code =
      """|package a
         |import scala.language.reflectiveCalls
         |object UseBox {
         |  val i = new Box[Int].v.get
         |  val s = new Box[String].v.get
         |}
         |class Box[T] { val v: { def get: T } = ??? }
         |""".stripMargin
    assertNoDiff(
      summary(document(compile(Map("Box.scala" -> code)), "Box.scala")),
      """|a/Box# => class Box[T] extends AnyRef { +2 decls }
         |  T => a/Box#[T]
         |  AnyRef => scala/AnyRef#
         |a/Box#[T] => typeparam T
         |a/Box#`<init>`(). => primary ctor <init>()
         |a/Box#v. => val method v: AnyRef { def get: T }
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |  T => a/Box#[T]
         |a/UseBox. => final object UseBox extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |a/UseBox.i. => val method i: Int
         |  Int => scala/Int#
         |a/UseBox.s. => val method s: String
         |  String => scala/Predef.String#
         |local0 => abstract method get: T
         |  T => a/Box#[T]
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:13): UseBox <= a/UseBox.
         |[3:6..3:7): i <= a/UseBox.i.
         |[3:14..3:17): Box => a/Box#
         |[3:18..3:21): Int => scala/Int#
         |[3:22..3:22):  => a/Box#`<init>`().
         |[3:23..3:24): v => a/Box#v.
         |[3:25..3:28): get => local0
         |[4:6..4:7): s <= a/UseBox.s.
         |[4:14..4:17): Box => a/Box#
         |[4:18..4:24): String => scala/Predef.String#
         |[4:25..4:25):  => a/Box#`<init>`().
         |[4:26..4:27): v => a/Box#v.
         |[4:28..4:31): get => local0
         |[6:6..6:9): Box <= a/Box#
         |[6:10..6:11): T <= a/Box#[T]
         |[6:13..6:13):  <= a/Box#`<init>`().
         |[6:19..6:20): v <= a/Box#v.
         |[6:28..6:31): get <= local0
         |[6:33..6:34): T => a/Box#[T]
         |[6:39..6:42): ??? => scala/Predef.`???`().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a refinement in a generic class at two type arguments, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(boxA, boxB, "UseBox.scala")),
      """|b/UseBox. => final object UseBox extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseBox.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseBox.s. => val method s: String
         |  String => scala/Predef.String#
         |local0 => abstract method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: String
         |  String => scala/Predef.String#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:13): UseBox <= b/UseBox.
         |[3:6..3:7): i <= b/UseBox.i.
         |[3:14..3:15): a => a/
         |[3:16..3:19): Box => a/Box#
         |[3:20..3:23): Int => scala/Int#
         |[3:24..3:24):  => a/Box#`<init>`().
         |[3:25..3:26): v => a/Box#v.
         |[3:27..3:30): get => local0
         |[4:6..4:7): s <= b/UseBox.s.
         |[4:14..4:15): a => a/
         |[4:16..4:19): Box => a/Box#
         |[4:20..4:26): String => scala/Predef.String#
         |[4:27..4:27):  => a/Box#`<init>`().
         |[4:28..4:29): v => a/Box#v.
         |[4:30..4:33): get => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a refinement in a generic class at two type arguments, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(boxA, boxB, "UseBox.scala")),
      """|b/UseBox. => final object UseBox extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseBox.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseBox.s. => val method s: String
         |  String => scala/Predef.String#
         |local0 => abstract method get: T
         |  T => a/Box#[T]
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:13): UseBox <= b/UseBox.
         |[3:6..3:7): i <= b/UseBox.i.
         |[3:14..3:15): a => a/
         |[3:16..3:19): Box => a/Box#
         |[3:20..3:23): Int => scala/Int#
         |[3:24..3:24):  => a/Box#`<init>`().
         |[3:25..3:26): v => a/Box#v.
         |[3:27..3:30): get => local0
         |[4:6..4:7): s <= b/UseBox.s.
         |[4:14..4:15): a => a/
         |[4:16..4:19): Box => a/Box#
         |[4:20..4:26): String => scala/Predef.String#
         |[4:27..4:27):  => a/Box#`<init>`().
         |[4:28..4:29): v => a/Box#v.
         |[4:30..4:33): get => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // subclasses narrow the refinement to classes of their own
  private val getterA = Map {
    "F.scala" ->
      """|package a
         |trait F[T] { def x: { def get: T } }
         |class IntGet { def get: Int = 0 }
         |class LongGet { def get: Long = 0 }
         |class IntF extends F[Int] { def x: IntGet = new IntGet }
         |class LongF extends F[Long] { def x: LongGet = new LongGet }
         |""".stripMargin
  }

  private val getterB = Map {
    "UseF.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object UseF {
         |  val i = new a.IntF().x.get
         |  val l = new a.LongF().x.get
         |  val fi = (new a.IntF: a.F[Int]).x.get
         |  val fl = (new a.LongF: a.F[Long]).x.get
         |}
         |""".stripMargin
  }

  test("#1591: a refinement in a generic trait, and classes in its subclasses, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(getterA, getterB, "UseF.scala")),
      """|b/UseF. => final object UseF extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseF.fi. => val method fi: Int
         |  Int => scala/Int#
         |b/UseF.fl. => val method fl: Long
         |  Long => scala/Long#
         |b/UseF.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseF.l. => val method l: Long
         |  Long => scala/Long#
         |local0 => abstract method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: Long
         |  Long => scala/Long#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseF <= b/UseF.
         |[3:6..3:7): i <= b/UseF.i.
         |[3:14..3:15): a => a/
         |[3:16..3:20): IntF => a/IntF#
         |[3:20..3:20):  => a/IntF#`<init>`().
         |[3:23..3:24): x => a/IntF#x().
         |[3:25..3:28): get => a/IntGet#get().
         |[4:6..4:7): l <= b/UseF.l.
         |[4:14..4:15): a => a/
         |[4:16..4:21): LongF => a/LongF#
         |[4:21..4:21):  => a/LongF#`<init>`().
         |[4:24..4:25): x => a/LongF#x().
         |[4:26..4:29): get => a/LongGet#get().
         |[5:6..5:8): fi <= b/UseF.fi.
         |[5:16..5:17): a => a/
         |[5:18..5:22): IntF => a/IntF#
         |[5:22..5:22):  => a/IntF#`<init>`().
         |[5:24..5:25): a => a/
         |[5:26..5:27): F => a/F#
         |[5:28..5:31): Int => scala/Int#
         |[5:34..5:35): x => a/F#x().
         |[5:36..5:39): get => local0
         |[6:6..6:8): fl <= b/UseF.fl.
         |[6:16..6:17): a => a/
         |[6:18..6:23): LongF => a/LongF#
         |[6:23..6:23):  => a/LongF#`<init>`().
         |[6:25..6:26): a => a/
         |[6:27..6:28): F => a/F#
         |[6:29..6:33): Long => scala/Long#
         |[6:36..6:37): x => a/F#x().
         |[6:38..6:41): get => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a refinement in a generic trait, and classes in its subclasses, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(getterA, getterB, "UseF.scala")),
      """|b/UseF. => final object UseF extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseF.fi. => val method fi: Int
         |  Int => scala/Int#
         |b/UseF.fl. => val method fl: Long
         |  Long => scala/Long#
         |b/UseF.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseF.l. => val method l: Long
         |  Long => scala/Long#
         |local0 => abstract method get: T
         |  T => a/F#[T]
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseF <= b/UseF.
         |[3:6..3:7): i <= b/UseF.i.
         |[3:14..3:15): a => a/
         |[3:16..3:20): IntF => a/IntF#
         |[3:20..3:20):  => a/IntF#`<init>`().
         |[3:23..3:24): x => a/IntF#x().
         |[3:25..3:28): get => a/IntGet#get().
         |[4:6..4:7): l <= b/UseF.l.
         |[4:14..4:15): a => a/
         |[4:16..4:21): LongF => a/LongF#
         |[4:21..4:21):  => a/LongF#`<init>`().
         |[4:24..4:25): x => a/LongF#x().
         |[4:26..4:29): get => a/LongGet#get().
         |[5:6..5:8): fi <= b/UseF.fi.
         |[5:16..5:17): a => a/
         |[5:18..5:22): IntF => a/IntF#
         |[5:22..5:22):  => a/IntF#`<init>`().
         |[5:24..5:25): a => a/
         |[5:26..5:27): F => a/F#
         |[5:28..5:31): Int => scala/Int#
         |[5:34..5:35): x => a/F#x().
         |[5:36..5:39): get => local0
         |[6:6..6:8): fl <= b/UseF.fl.
         |[6:16..6:17): a => a/
         |[6:18..6:23): LongF => a/LongF#
         |[6:23..6:23):  => a/LongF#`<init>`().
         |[6:25..6:26): a => a/
         |[6:27..6:28): F => a/F#
         |[6:29..6:33): Long => scala/Long#
         |[6:36..6:37): x => a/F#x().
         |[6:38..6:41): get => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // a pattern skolemizes an existential of A at the position of the existential
  private val existentialA = Map(
    "E.scala" ->
      """|package a
         |object E {
         |  class W[T](val t: T) { def self: W[T] = this }
         |  val w: W[_] = ???
         |}
         |""".stripMargin,
  )

  private val patternB = Map(
    "UseE.scala" ->
      """|package b
         |object UseE {
         |  def m = a.E.w match { case v: a.E.W[t] => v.t }
         |}
         |""".stripMargin,
  )

  private val typeThenPatternB = Map(
    "UseE.scala" ->
      """|package b
         |object UseE {
         |  val s = a.E.w.self
         |  def m = a.E.w match { case v: a.E.W[t] => v.t }
         |}
         |""".stripMargin,
  )

  private val patternThenTypeB = Map(
    "UseE.scala" ->
      """|package b
         |object UseE {
         |  def m = a.E.w match { case v: a.E.W[t] => v.t }
         |  val s = a.E.w.self
         |}
         |""".stripMargin,
  )

  private val twoPatternsB = Map(
    "UseE.scala" ->
      """|package b
         |object UseE {
         |  def m = a.E.w match { case v: a.E.W[t] => v.t }
         |  def n = a.E.w match { case u: a.E.W[r] => u.t }
         |}
         |""".stripMargin,
  )

  private val typeThenPatternE = Map(
    "E.scala" ->
      """|package a
         |object E {
         |  class W[T](val t: T) { def self: W[T] = this }
         |  val w: W[_] = ???
         |  val s = w.self
         |  def m = w match { case v: W[t] => v.t }
         |}
         |""".stripMargin,
  )

  private val patternThenTypeE = Map(
    "E.scala" ->
      """|package a
         |object E {
         |  class W[T](val t: T) { def self: W[T] = this }
         |  val w: W[_] = ???
         |  def m = w match { case v: W[t] => v.t }
         |  val s = w.self
         |}
         |""".stripMargin,
  )

  private val twoPatternsE = Map(
    "E.scala" ->
      """|package a
         |object E {
         |  class W[T](val t: T) { def self: W[T] = this }
         |  val w: W[_] = ???
         |  def m = w match { case v: W[t] => v.t }
         |  def n = w match { case u: W[r] => u.t }
         |}
         |""".stripMargin,
  )

  test("an existential of another file of the run, in a pattern") {
    assertNoDiff(
      summary(inOneRun(existentialA, patternB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local1 => synthetic abstract type _
         |local2 => abstract type t
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): m <= b/UseE.m().
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:29..2:30): v <= local0
         |[2:32..2:33): a => a/
         |[2:34..2:35): E => a/E.
         |[2:36..2:37): W => a/E.W#
         |[2:38..2:39): t <= local2
         |[2:44..2:45): v => local0
         |[2:46..2:47): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential of another file of the run, in a type, then in a pattern") {
    assertNoDiff(
      summary(inOneRun(existentialA, typeThenPatternB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |b/UseE.s. => val method s: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |local0 => synthetic abstract type _
         |local1 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local0
         |local2 => abstract type t
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): s <= b/UseE.s.
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:16..2:20): self => a/E.W#self().
         |[3:6..3:7): m <= b/UseE.m().
         |[3:10..3:11): a => a/
         |[3:12..3:13): E => a/E.
         |[3:14..3:15): w => a/E.w.
         |[3:29..3:30): v <= local1
         |[3:32..3:33): a => a/
         |[3:34..3:35): E => a/E.
         |[3:36..3:37): W => a/E.W#
         |[3:38..3:39): t <= local2
         |[3:44..3:45): v => local1
         |[3:46..3:47): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential of another file of the run, in a pattern, then in a type") {
    assertNoDiff(
      summary(inOneRun(existentialA, patternThenTypeB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |b/UseE.s. => val method s: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local1
         |local0 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local1 => synthetic abstract type _
         |local2 => abstract type t
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): m <= b/UseE.m().
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:29..2:30): v <= local0
         |[2:32..2:33): a => a/
         |[2:34..2:35): E => a/E.
         |[2:36..2:37): W => a/E.W#
         |[2:38..2:39): t <= local2
         |[2:44..2:45): v => local0
         |[2:46..2:47): t => a/E.W#t.
         |[3:6..3:7): s <= b/UseE.s.
         |[3:10..3:11): a => a/
         |[3:12..3:13): E => a/E.
         |[3:14..3:15): w => a/E.w.
         |[3:16..3:20): self => a/E.W#self().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential of another file of the run, in two patterns") {
    assertNoDiff(
      summary(inOneRun(existentialA, twoPatternsB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |b/UseE.n(). => method n: Any
         |  Any => scala/Any#
         |local0 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local1 => synthetic abstract type _
         |local2 => abstract type t
         |local3 => val local u: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local4 => abstract type r
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): m <= b/UseE.m().
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:29..2:30): v <= local0
         |[2:32..2:33): a => a/
         |[2:34..2:35): E => a/E.
         |[2:36..2:37): W => a/E.W#
         |[2:38..2:39): t <= local2
         |[2:44..2:45): v => local0
         |[2:46..2:47): t => a/E.W#t.
         |[3:6..3:7): n <= b/UseE.n().
         |[3:10..3:11): a => a/
         |[3:12..3:13): E => a/E.
         |[3:14..3:15): w => a/E.w.
         |[3:29..3:30): u <= local3
         |[3:32..3:33): a => a/
         |[3:34..3:35): E => a/E.
         |[3:36..3:37): W => a/E.W#
         |[3:38..3:39): r <= local4
         |[3:44..3:45): u => local3
         |[3:46..3:47): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential of the same file, in a type, then in a pattern") {
    assertNoDiff(
      summary(document(compile(typeThenPatternE), "E.scala")),
      """|a/E. => final object E extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |a/E.W# => class W[T] extends AnyRef { +3 decls }
         |  T => a/E.W#[T]
         |  AnyRef => scala/AnyRef#
         |a/E.W#[T] => typeparam T
         |a/E.W#`<init>`(). => primary ctor <init>(val t: T)
         |  t => a/E.W#`<init>`().(t)
         |  T => a/E.W#[T]
         |a/E.W#`<init>`().(t) => val param t: T
         |  T => a/E.W#[T]
         |a/E.W#self(). => method self: W[T]
         |  W => a/E.W#
         |  T => a/E.W#[T]
         |a/E.W#t. => val method t: T
         |  T => a/E.W#[T]
         |a/E.m(). => method m: Any
         |  Any => scala/Any#
         |a/E.s. => val method s: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |a/E.w. => val method w: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |local0 => synthetic abstract type _
         |local1 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local0
         |local2 => abstract type t
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:8): E <= a/E.
         |[2:8..2:9): W <= a/E.W#
         |[2:10..2:11): T <= a/E.W#[T]
         |[2:12..2:12):  <= a/E.W#`<init>`().
         |[2:17..2:18): t <= a/E.W#`<init>`().(t)
         |[2:17..2:18): t <= a/E.W#t.
         |[2:20..2:21): T => a/E.W#[T]
         |[2:29..2:33): self <= a/E.W#self().
         |[2:35..2:36): W => a/E.W#
         |[2:37..2:38): T => a/E.W#[T]
         |[3:6..3:7): w <= a/E.w.
         |[3:9..3:10): W => a/E.W#
         |[3:11..3:12): _ <= local0
         |[3:16..3:19): ??? => scala/Predef.`???`().
         |[4:6..4:7): s <= a/E.s.
         |[4:10..4:11): w => a/E.w.
         |[4:12..4:16): self => a/E.W#self().
         |[5:6..5:7): m <= a/E.m().
         |[5:10..5:11): w => a/E.w.
         |[5:25..5:26): v <= local1
         |[5:28..5:29): W => a/E.W#
         |[5:30..5:31): t <= local2
         |[5:36..5:37): v => local1
         |[5:38..5:39): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential of the same file, in a pattern, then in a type") {
    assertNoDiff(
      summary(document(compile(patternThenTypeE), "E.scala")),
      """|a/E. => final object E extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |a/E.W# => class W[T] extends AnyRef { +3 decls }
         |  T => a/E.W#[T]
         |  AnyRef => scala/AnyRef#
         |a/E.W#[T] => typeparam T
         |a/E.W#`<init>`(). => primary ctor <init>(val t: T)
         |  t => a/E.W#`<init>`().(t)
         |  T => a/E.W#[T]
         |a/E.W#`<init>`().(t) => val param t: T
         |  T => a/E.W#[T]
         |a/E.W#self(). => method self: W[T]
         |  W => a/E.W#
         |  T => a/E.W#[T]
         |a/E.W#t. => val method t: T
         |  T => a/E.W#[T]
         |a/E.m(). => method m: Any
         |  Any => scala/Any#
         |a/E.s. => val method s: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |a/E.w. => val method w: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |local0 => synthetic abstract type _
         |local1 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local0
         |local2 => abstract type t
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:8): E <= a/E.
         |[2:8..2:9): W <= a/E.W#
         |[2:10..2:11): T <= a/E.W#[T]
         |[2:12..2:12):  <= a/E.W#`<init>`().
         |[2:17..2:18): t <= a/E.W#`<init>`().(t)
         |[2:17..2:18): t <= a/E.W#t.
         |[2:20..2:21): T => a/E.W#[T]
         |[2:29..2:33): self <= a/E.W#self().
         |[2:35..2:36): W => a/E.W#
         |[2:37..2:38): T => a/E.W#[T]
         |[3:6..3:7): w <= a/E.w.
         |[3:9..3:10): W => a/E.W#
         |[3:11..3:12): _ <= local0
         |[3:16..3:19): ??? => scala/Predef.`???`().
         |[4:6..4:7): m <= a/E.m().
         |[4:10..4:11): w => a/E.w.
         |[4:25..4:26): v <= local1
         |[4:28..4:29): W => a/E.W#
         |[4:30..4:31): t <= local2
         |[4:36..4:37): v => local1
         |[4:38..4:39): t => a/E.W#t.
         |[5:6..5:7): s <= a/E.s.
         |[5:10..5:11): w => a/E.w.
         |[5:12..5:16): self => a/E.W#self().
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential of the same file, in two patterns") {
    assertNoDiff(
      summary(document(compile(twoPatternsE), "E.scala")),
      """|a/E. => final object E extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |a/E.W# => class W[T] extends AnyRef { +3 decls }
         |  T => a/E.W#[T]
         |  AnyRef => scala/AnyRef#
         |a/E.W#[T] => typeparam T
         |a/E.W#`<init>`(). => primary ctor <init>(val t: T)
         |  t => a/E.W#`<init>`().(t)
         |  T => a/E.W#[T]
         |a/E.W#`<init>`().(t) => val param t: T
         |  T => a/E.W#[T]
         |a/E.W#self(). => method self: W[T]
         |  W => a/E.W#
         |  T => a/E.W#[T]
         |a/E.W#t. => val method t: T
         |  T => a/E.W#[T]
         |a/E.m(). => method m: Any
         |  Any => scala/Any#
         |a/E.n(). => method n: Any
         |  Any => scala/Any#
         |a/E.w. => val method w: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |local0 => synthetic abstract type _
         |local1 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local0
         |local2 => abstract type t
         |local3 => val local u: W[_]
         |  W => a/E.W#
         |  _ => local0
         |local4 => abstract type r
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:8): E <= a/E.
         |[2:8..2:9): W <= a/E.W#
         |[2:10..2:11): T <= a/E.W#[T]
         |[2:12..2:12):  <= a/E.W#`<init>`().
         |[2:17..2:18): t <= a/E.W#`<init>`().(t)
         |[2:17..2:18): t <= a/E.W#t.
         |[2:20..2:21): T => a/E.W#[T]
         |[2:29..2:33): self <= a/E.W#self().
         |[2:35..2:36): W => a/E.W#
         |[2:37..2:38): T => a/E.W#[T]
         |[3:6..3:7): w <= a/E.w.
         |[3:9..3:10): W => a/E.W#
         |[3:11..3:12): _ <= local0
         |[3:16..3:19): ??? => scala/Predef.`???`().
         |[4:6..4:7): m <= a/E.m().
         |[4:10..4:11): w => a/E.w.
         |[4:25..4:26): v <= local1
         |[4:28..4:29): W => a/E.W#
         |[4:30..4:31): t <= local2
         |[4:36..4:37): v => local1
         |[4:38..4:39): t => a/E.W#t.
         |[5:6..5:7): n <= a/E.n().
         |[5:10..5:11): w => a/E.w.
         |[5:25..5:26): u <= local3
         |[5:28..5:29): W => a/E.W#
         |[5:30..5:31): r <= local4
         |[5:36..5:37): u => local3
         |[5:38..5:39): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential from a classfile, in a pattern") {
    assertNoDiff(
      summary(fromClassfile(existentialA, patternB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local1 => abstract type _
         |local2 => abstract type t
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): m <= b/UseE.m().
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:29..2:30): v <= local0
         |[2:32..2:33): a => a/
         |[2:34..2:35): E => a/E.
         |[2:36..2:37): W => a/E.W#
         |[2:38..2:39): t <= local2
         |[2:44..2:45): v => local0
         |[2:46..2:47): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential from a classfile, in a type, then in a pattern") {
    assertNoDiff(
      summary(fromClassfile(existentialA, typeThenPatternB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |b/UseE.s. => val method s: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local0
         |local1 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local2
         |local2 => abstract type _
         |local3 => abstract type t
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): s <= b/UseE.s.
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:16..2:20): self => a/E.W#self().
         |[3:6..3:7): m <= b/UseE.m().
         |[3:10..3:11): a => a/
         |[3:12..3:13): E => a/E.
         |[3:14..3:15): w => a/E.w.
         |[3:29..3:30): v <= local1
         |[3:32..3:33): a => a/
         |[3:34..3:35): E => a/E.
         |[3:36..3:37): W => a/E.W#
         |[3:38..3:39): t <= local3
         |[3:44..3:45): v => local1
         |[3:46..3:47): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List(local0)
         |""".stripMargin,
    )
  }

  test("an existential from a classfile, in a pattern, then in a type") {
    assertNoDiff(
      summary(fromClassfile(existentialA, patternThenTypeB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |b/UseE.s. => val method s: W[_] forSome { type _ }
         |  W => a/E.W#
         |  _ => local3
         |local0 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local1 => abstract type _
         |local2 => abstract type t
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): m <= b/UseE.m().
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:29..2:30): v <= local0
         |[2:32..2:33): a => a/
         |[2:34..2:35): E => a/E.
         |[2:36..2:37): W => a/E.W#
         |[2:38..2:39): t <= local2
         |[2:44..2:45): v => local0
         |[2:46..2:47): t => a/E.W#t.
         |[3:6..3:7): s <= b/UseE.s.
         |[3:10..3:11): a => a/
         |[3:12..3:13): E => a/E.
         |[3:14..3:15): w => a/E.w.
         |[3:16..3:20): self => a/E.W#self().
         |----
         |collisions List()
         |dangling List(local3)
         |""".stripMargin,
    )
  }

  test("an existential from a classfile, in two patterns") {
    assertNoDiff(
      summary(fromClassfile(existentialA, twoPatternsB, "UseE.scala")),
      """|b/UseE. => final object UseE extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseE.m(). => method m: Any
         |  Any => scala/Any#
         |b/UseE.n(). => method n: Any
         |  Any => scala/Any#
         |local0 => val local v: W[_]
         |  W => a/E.W#
         |  _ => local1
         |local1 => abstract type _
         |local2 => abstract type t
         |local3 => val local u: W[_]
         |  W => a/E.W#
         |  _ => local4
         |local4 => abstract type _
         |local5 => abstract type r
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:11): UseE <= b/UseE.
         |[2:6..2:7): m <= b/UseE.m().
         |[2:10..2:11): a => a/
         |[2:12..2:13): E => a/E.
         |[2:14..2:15): w => a/E.w.
         |[2:29..2:30): v <= local0
         |[2:32..2:33): a => a/
         |[2:34..2:35): E => a/E.
         |[2:36..2:37): W => a/E.W#
         |[2:38..2:39): t <= local2
         |[2:44..2:45): v => local0
         |[2:46..2:47): t => a/E.W#t.
         |[3:6..3:7): n <= b/UseE.n().
         |[3:10..3:11): a => a/
         |[3:12..3:13): E => a/E.
         |[3:14..3:15): w => a/E.w.
         |[3:29..3:30): u <= local3
         |[3:32..3:33): a => a/
         |[3:34..3:35): E => a/E.
         |[3:36..3:37): W => a/E.W#
         |[3:38..3:39): r <= local5
         |[3:44..3:45): u => local3
         |[3:46..3:47): t => a/E.W#t.
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("a SAM lambda, in the same file") {
    val sam = Map(
      "S.scala" ->
        """|package a
           |object O {
           |  trait S { def f(a: Int): Int }
           |  def w(s: S) = s.f(0)
           |  w(_ + 1)
           |}
           |""".stripMargin,
    )
    val doc = document(compile(sam), "S.scala")
    val decls = doc.symbols.find(_.symbol == "local0").map(_.signature) match {
      case Some(sig: s.ClassSignature) => sig.declarations.toList.flatMap(_.symlinks)
      case _ => Nil
    }
    assertEquals(decls, List("local1"))
    assertNoDiff(
      summary(doc),
      """|a/O. => final object O extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |a/O.S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |a/O.S#f(). => abstract method f(a: Int): Int
         |  a => a/O.S#f().(a)
         |  Int => scala/Int#
         |a/O.S#f().(a) => param a: Int
         |  Int => scala/Int#
         |a/O.w(). => method w(s: S): Int
         |  s => a/O.w().(s)
         |  S => a/O.S#
         |  Int => scala/Int#
         |a/O.w().(s) => param s: S
         |  S => a/O.S#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => a/O.S#
         |local1 => synthetic method f(a: Int): Int <: a/O.S#f().
         |  a => a/O.S#f().(a)
         |  Int => scala/Int#
         |local2 => synthetic param x$1: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:8): O <= a/O.
         |[2:8..2:9): S <= a/O.S#
         |[2:16..2:17): f <= a/O.S#f().
         |[2:18..2:19): a <= a/O.S#f().(a)
         |[2:21..2:24): Int => scala/Int#
         |[2:27..2:30): Int => scala/Int#
         |[3:6..3:7): w <= a/O.w().
         |[3:8..3:9): s <= a/O.w().(s)
         |[3:11..3:12): S => a/O.S#
         |[3:16..3:17): s => a/O.w().(s)
         |[3:18..3:19): f => a/O.S#f().
         |[4:2..4:3): w => a/O.w().
         |[4:4..4:5): _ <= local2
         |[4:4..4:9): _ + 1 <= local0
         |[4:4..4:9): _ + 1 <= local1
         |[4:6..4:7): + => scala/Int#`+`(+4).
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // the trees of a macro expansion have the position of its call
  private val macroA = Map {
    "M.scala" ->
      """|package a
         |import scala.language.experimental.macros
         |import scala.reflect.macros.whitebox
         |object M {
         |  def pair: Any = macro impl
         |  def impl(c: whitebox.Context): c.Tree = {
         |    import c.universe._
         |    q"(new { def x = 1 }, new { def x = 2L })"
         |  }
         |}
         |""".stripMargin
  }

  private val macroB = Map(
    "U.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object U {
         |  val p = a.M.pair
         |  val i = p._1.x
         |  val l = p._2.x
         |}
         |""".stripMargin,
  )

  test("#1591: two refinements of one macro expansion") {
    assertNoDiff(
      summary(fromClassfile(macroA, macroB, "U.scala")),
      """|b/U. => final object U extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |b/U.i. => val method i: Int
         |  Int => scala/Int#
         |b/U.l. => val method l: Long
         |  Long => scala/Long#
         |b/U.p. => val method p: Tuple2[AnyRef { def x: Int }, AnyRef { def x: Long }]
         |  Tuple2 => scala/Tuple2#
         |  AnyRef => scala/AnyRef#
         |  x => local4
         |  Int => scala/Int#
         |  x => local5
         |  Long => scala/Long#
         |local0 => final class $anon extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local1 => method x: Int
         |  Int => scala/Int#
         |local2 => final class $anon extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local3 => method x: Long
         |  Long => scala/Long#
         |local4 => method x: Int
         |  Int => scala/Int#
         |local5 => method x: Long
         |  Long => scala/Long#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:8): U <= b/U.
         |[3:6..3:7): p <= b/U.p.
         |[3:10..3:11): a => a/
         |[3:12..3:13): M => a/M.
         |[3:14..3:14):  <= local2
         |[3:14..3:18): pair => a/M.pair().
         |[4:6..4:7): i <= b/U.i.
         |[4:10..4:11): p => b/U.p.
         |[4:12..4:14): _1 => scala/Tuple2#_1.
         |[4:15..4:16): x => local4
         |[5:6..5:7): l <= b/U.l.
         |[5:10..5:11): p => b/U.p.
         |[5:12..5:14): _2 => scala/Tuple2#_2.
         |[5:15..5:16): x => local5
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  private val macroOneA = Map {
    "M1.scala" ->
      """|package a
         |import scala.language.experimental.macros
         |import scala.reflect.macros.whitebox
         |object M1 {
         |  def one: Any = macro impl
         |  def impl(c: whitebox.Context): c.Tree = {
         |    import c.universe._
         |    q"new { def x = 1 }"
         |  }
         |}
         |""".stripMargin
  }

  test("#1591: one refinement of a macro expansion") {
    val b = Map(
      "U.scala" ->
        """|package b
           |import scala.language.reflectiveCalls
           |object U {
           |  val o = a.M1.one
           |  val i = o.x
           |}
           |""".stripMargin,
    )
    assertNoDiff(
      summary(fromClassfile(macroOneA, b, "U.scala")),
      """|b/U. => final object U extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/U.i. => val method i: Int
         |  Int => scala/Int#
         |b/U.o. => val method o: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local1
         |  Int => scala/Int#
         |local0 => final class $anon extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local1 => method x: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:8): U <= b/U.
         |[3:6..3:7): o <= b/U.o.
         |[3:10..3:11): a => a/
         |[3:12..3:14): M1 => a/M1.
         |[3:15..3:15):  <= local0
         |[3:15..3:18): one => a/M1.one().
         |[4:6..4:7): i <= b/U.i.
         |[4:10..4:11): o => b/U.o.
         |[4:12..4:13): x => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: two refinements of one macro expansion, in another file of the run") {
    val b = Map(
      "P.scala" ->
        """|package b
           |object P {
           |  val p = a.M.pair
           |}
           |""".stripMargin,
      "U.scala" ->
        """|package b
           |import scala.language.reflectiveCalls
           |object U {
           |  val i = P.p._1.x
           |  val l = P.p._2.x
           |}
           |""".stripMargin,
    )
    assertNoDiff(
      summary(fromClassfile(macroA, b, "U.scala")),
      """|b/U. => final object U extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/U.i. => val method i: Int
         |  Int => scala/Int#
         |b/U.l. => val method l: Long
         |  Long => scala/Long#
         |local0 => method x: Int
         |  Int => scala/Int#
         |local1 => method x: Long
         |  Long => scala/Long#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:8): U <= b/U.
         |[3:6..3:7): i <= b/U.i.
         |[3:10..3:11): P => b/P.
         |[3:12..3:13): p => b/P.p.
         |[3:14..3:16): _1 => scala/Tuple2#_1.
         |[3:17..3:18): x => local0
         |[4:6..4:7): l <= b/U.l.
         |[4:10..4:11): P => b/P.
         |[4:12..4:13): p => b/P.p.
         |[4:14..4:16): _2 => scala/Tuple2#_2.
         |[4:17..4:18): x => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("a local outside a document") {
    val file = Files.createTempDirectory("issue1591_").resolve("O.scala")
    Files.write(file, "object O { def f = { val x = 1; x } }".getBytes("UTF-8"))
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.processArgumentString("-Ystop-after:typer")
    val pc = new Global(settings, new StoreReporter())
    val run = new pc.Run()
    run.compile(List(file.toString))
    // Metals asks for the symbol of a tree without a document
    val ops = new SemanticdbOps {
      val global: pc.type = pc
    }
    import ops._
    val x = run.units.next().body
      .collect { case t: pc.ValDef if t.name.toString == "x" => t.symbol }
    assertEquals(x.map(_.toSemantic), List("local0"))
  }

  // a compiler that stops after typer, as Metals runs it
  private class Typed(code: String) {

    private val dir = Files.createTempDirectory("issue1591_")
    private val file = dir.resolve("O.scala")
    Files.write(file, code.getBytes("UTF-8"))
    val pc: typer.type = typer
    private val run = newRun(pc)
    run.compile(List(file.toString))
    private val unit = run.units.next()
    val ops = new SemanticdbOps {
      val global: pc.type = pc
    }
    ops.config = ops.config.copy(sourceroot = AbsolutePath(dir))
    import ops._
    def locals(name: String): List[String] = unit.body
      .collect { case t: pc.ValDef if t.name.toString == name => t.symbol.toSemantic }
    def document: s.TextDocument = unit.toTextDocument
    // the existential types in the types of the trees
    def existentials: List[String] = {
      val syms = mutable.LinkedHashSet.empty[pc.Symbol]
      unit.body
        .foreach(t => if (t.tpe != null) t.tpe.foreach(tpe => syms ++= tpe.typeArgs.map(_.typeSymbol)))
      syms.toList.filter(_.hasFlag(scala.reflect.internal.Flags.EXISTENTIAL)).map(_.toSemantic)
    }

  }

  test("two locals of one name outside a document") {
    val code = "object O { def f = { val x = 1; x }; def g = { val x = 2; x } }"
    assertEquals(new Typed(code).locals("x"), List("local0", "local1"))
  }

  test("a local outside a document, before and after a document") {
    val code = "object O { def f = { val y = 0; val x = 1; x + y } }"
    val typed = new Typed(code)
    val before = typed.locals("x")
    val doc = typed.document
    assertEquals(before ++ typed.locals("x"), List("local0", "local1"))
    assertNoDiff(
      symbols(doc),
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.f(). => method f: Int
         |  Int => scala/Int#
         |local0 => val local y: Int
         |  Int => scala/Int#
         |local1 => val local x: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  // the presentation compiler leaves a function untyped if its application fails
  test("an existential skolem beside an untyped function, in the presentation compiler") {
    val code =
      """|object O {
         |  val v = Option(Class.forName("C"))
         |  def h(i: Int) = i
         |  val f = h((x: Int) => x)
         |}
         |""".stripMargin
    val pc = InteractiveSemanticdb.newCompiler()
    val options = List("-P:semanticdb:synthetics:on")
    val doc = InteractiveSemanticdb.toTextDocument(pc, code, options)
    pc.askShutdown()
    assertNoDiff(
      section(doc, "Synthetics"),
      """|[1:10..1:16) => *.apply[Class[?0]]
         |  apply => scala/Option.apply().
         |  Class => java/lang/Class#
         |  ?0 => local0
         |""".stripMargin,
    )
  }

  // the SymbolInformation of the locals of a document, which Symbols prints last
  private def locals(doc: s.TextDocument): String = symbols(doc).linesIterator
    .dropWhile(!_.startsWith("local")).mkString("\n")

  // a member of a refinement that refers to a local class of its method
  private val escapeA = Map(
    "Esc.scala" ->
      """|package a
         |object Esc {
         |  def f = { class K { def get = 1 }; new K }
         |  def g = { class K2; val r: { def get: K2 } = ???; r }
         |}
         |""".stripMargin,
  )

  private val escapeB = Map(
    "UseEsc.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object UseEsc {
         |  val x = a.Esc.f.get
         |  val y = a.Esc.g
         |  val z = a.Esc.g.get
         |}
         |""".stripMargin,
  )

  test("#1591: a refinement that refers to a local class of its method, in the same file") {
    val code = escapeA("Esc.scala") + escapeB("UseEsc.scala").replace("package b\n", "")
    assertNoDiff(
      locals(document(compile(Map("Esc.scala" -> code)), "Esc.scala")),
      """|local0 => method get: Int
         |  Int => scala/Int#
         |local1 => class K extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |local2 => primary ctor <init>()
         |local3 => abstract method get: K2
         |  K2 => local4
         |local4 => class K2 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local5 => primary ctor <init>()
         |local6 => val local r: AnyRef { def get: K2 }
         |  AnyRef => scala/AnyRef#
         |  get => local3
         |  K2 => local4
         |""".stripMargin,
    )
  }

  test("#1591: a refinement that refers to a local class of its method, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(escapeA, escapeB, "UseEsc.scala")),
      """|b/UseEsc. => final object UseEsc extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseEsc.x. => val method x: Int
         |  Int => scala/Int#
         |b/UseEsc.y. => val method y: Object { def get: K2 }
         |  Object => java/lang/Object#
         |  get => local1
         |  K2 => local2
         |b/UseEsc.z. => val method z: AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: K2
         |  K2 => local2
         |local2 => class K2 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local3 => primary ctor <init>()
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:13): UseEsc <= b/UseEsc.
         |[3:6..3:7): x <= b/UseEsc.x.
         |[3:10..3:11): a => a/
         |[3:12..3:15): Esc => a/Esc.
         |[3:16..3:17): f => a/Esc.f().
         |[3:18..3:21): get => local0
         |[4:6..4:7): y <= b/UseEsc.y.
         |[4:10..4:11): a => a/
         |[4:12..4:15): Esc => a/Esc.
         |[4:16..4:17): g => a/Esc.g().
         |[5:6..5:7): z <= b/UseEsc.z.
         |[5:10..5:11): a => a/
         |[5:12..5:15): Esc => a/Esc.
         |[5:16..5:17): g => a/Esc.g().
         |[5:18..5:21): get => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a refinement that refers to a local class of its method, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(escapeA, escapeB, "UseEsc.scala")),
      """|b/UseEsc. => final object UseEsc extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseEsc.x. => val method x: Int
         |  Int => scala/Int#
         |b/UseEsc.y. => val method y: Object { def get: AnyRef }
         |  Object => java/lang/Object#
         |  get => local1
         |  AnyRef => scala/AnyRef#
         |b/UseEsc.z. => val method z: AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: AnyRef
         |  AnyRef => scala/AnyRef#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:13): UseEsc <= b/UseEsc.
         |[3:6..3:7): x <= b/UseEsc.x.
         |[3:10..3:11): a => a/
         |[3:12..3:15): Esc => a/Esc.
         |[3:16..3:17): f => a/Esc.f().
         |[3:18..3:21): get => local0
         |[4:6..4:7): y <= b/UseEsc.y.
         |[4:10..4:11): a => a/
         |[4:12..4:15): Esc => a/Esc.
         |[4:16..4:17): g => a/Esc.g().
         |[5:6..5:7): z <= b/UseEsc.z.
         |[5:10..5:11): a => a/
         |[5:12..5:15): Esc => a/Esc.
         |[5:16..5:17): g => a/Esc.g().
         |[5:18..5:21): get => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // a refinement in the parameter of a generic case class, which scalac copies into `apply`
  private val caseA = Map(
    "K.scala" ->
      """|package a
         |case class K[T](r: { def get: T })
         |""".stripMargin,
  )

  private val caseB = Map(
    "UseK.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object UseK {
         |  val f = a.K.apply[Int] _
         |  val g = a.K[Int](null).r.get
         |}
         |""".stripMargin,
  )

  test("#1591: a refinement in the parameter of a generic case class, in the same file") {
    val code = caseA("K.scala") + caseB("UseK.scala").replace("package b\n", "")
    assertNoDiff(
      locals(document(compile(Map("K.scala" -> code)), "K.scala")),
      """|local0 => abstract method get: T
         |  T => a/K#[T]
         |local1 => synthetic val local K$1: K[T]
         |  K => a/K#
         |  T => a/K#[T]
         |""".stripMargin,
    )
  }

  test("#1591: a refinement in the parameter of a generic case class, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(caseA, caseB, "UseK.scala")),
      """|b/UseK. => final object UseK extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseK.f. => val method f: Function1[AnyRef { def get: T }, K[Int]]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |  T => a/K#[T]
         |  K => a/K#
         |  Int => scala/Int#
         |b/UseK.g. => val method g: Int
         |  Int => scala/Int#
         |local0 => abstract method get: T
         |  T => a/K#[T]
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseK <= b/UseK.
         |[3:6..3:7): f <= b/UseK.f.
         |[3:10..3:11): a => a/
         |[3:12..3:13): K => a/K.
         |[3:14..3:19): apply => a/K.apply().
         |[3:20..3:23): Int => scala/Int#
         |[4:6..4:7): g <= b/UseK.g.
         |[4:10..4:11): a => a/
         |[4:12..4:13): K => a/K.
         |[4:14..4:17): Int => scala/Int#
         |[4:25..4:26): r => a/K#r.
         |[4:27..4:30): get => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: a refinement in the parameter of a generic case class, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(caseA, caseB, "UseK.scala")),
      """|b/UseK. => final object UseK extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseK.f. => val method f: Function1[AnyRef { def get: Int }, K[Int]]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |  Int => scala/Int#
         |  K => a/K#
         |b/UseK.g. => val method g: Int
         |  Int => scala/Int#
         |local0 => abstract method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseK <= b/UseK.
         |[3:6..3:7): f <= b/UseK.f.
         |[3:10..3:11): a => a/
         |[3:12..3:13): K => a/K.
         |[3:14..3:19): apply => a/K.apply().
         |[3:20..3:23): Int => scala/Int#
         |[4:6..4:7): g <= b/UseK.g.
         |[4:10..4:11): a => a/
         |[4:12..4:13): K => a/K.
         |[4:14..4:17): Int => scala/Int#
         |[4:25..4:26): r => a/K#r.
         |[4:27..4:30): get => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // refinements in parameters that scalac copies into synthetic methods at the same position
  private val copiesA = Map {
    "M.scala" ->
      """|package a
         |import scala.language.reflectiveCalls
         |object M {
         |  case class D[T](r: { def get: T } = null)
         |  object E
         |  case class E[T](r: { def get: T })
         |  object F
         |  case class F[T](r: { def get: T } = null)
         |  class P[T](val r: { def get: T } = null)
         |  def m[T](r: { def get: T } = null): T = r.get
         |  implicit class I[T](val r: { def get: T })
         |}
         |""".stripMargin
  }

  private val copiesB = Map {
    "UseM.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object UseM {
         |  val d = a.M.D[Int]().r.get
         |  val e = a.M.E[Int](null).r.get
         |  val f = a.M.F[Int]().r.get
         |  val p = new a.M.P[Int]().r.get
         |  val i = a.M.I[Int](null).r.get
         |}
         |""".stripMargin
  }

  test("#1591: refinements in parameters that scalac copies, in the same file") {
    val code = copiesA("M.scala") + copiesB("UseM.scala").replace("package b\n", "")
    assertNoDiff(
      locals(document(compile(Map("M.scala" -> code)), "M.scala")),
      """|local0 => abstract method get: T
         |  T => a/M.D#[T]
         |local1 => synthetic val local D$1: D[T]
         |  D => a/M.D#
         |  T => a/M.D#[T]
         |local2 => abstract method get: T
         |  T => a/M.E#[T]
         |local3 => synthetic val local E$1: E[T]
         |  E => a/M.E#
         |  T => a/M.E#[T]
         |local4 => abstract method get: T
         |  T => a/M.F#[T]
         |local5 => synthetic val local F$1: F[T]
         |  F => a/M.F#
         |  T => a/M.F#[T]
         |local6 => abstract method get: T
         |  T => a/M.P#[T]
         |local7 => abstract method get: T
         |  T => a/M.m().[T]
         |local8 => abstract method get: T
         |  T => a/M.I#[T]
         |""".stripMargin,
    )
  }

  test("#1591: refinements in parameters that scalac copies, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(copiesA, copiesB, "UseM.scala")),
      """|b/UseM. => final object UseM extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseM.d. => val method d: Int
         |  Int => scala/Int#
         |b/UseM.e. => val method e: Int
         |  Int => scala/Int#
         |b/UseM.f. => val method f: Int
         |  Int => scala/Int#
         |b/UseM.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseM.p. => val method p: Int
         |  Int => scala/Int#
         |local0 => abstract method get: T
         |  T => a/M.D#[T]
         |local1 => abstract method get: T
         |  T => a/M.E#[T]
         |local2 => abstract method get: T
         |  T => a/M.F#[T]
         |local3 => abstract method get: T
         |  T => a/M.P#[T]
         |local4 => abstract method get: T
         |  T => a/M.I#[T]
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseM <= b/UseM.
         |[3:6..3:7): d <= b/UseM.d.
         |[3:10..3:11): a => a/
         |[3:12..3:13): M => a/M.
         |[3:14..3:15): D => a/M.D.
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): r => a/M.D#r.
         |[3:25..3:28): get => local0
         |[4:6..4:7): e <= b/UseM.e.
         |[4:10..4:11): a => a/
         |[4:12..4:13): M => a/M.
         |[4:14..4:15): E => a/M.E.
         |[4:16..4:19): Int => scala/Int#
         |[4:27..4:28): r => a/M.E#r.
         |[4:29..4:32): get => local1
         |[5:6..5:7): f <= b/UseM.f.
         |[5:10..5:11): a => a/
         |[5:12..5:13): M => a/M.
         |[5:14..5:15): F => a/M.F.
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:24): r => a/M.F#r.
         |[5:25..5:28): get => local2
         |[6:6..6:7): p <= b/UseM.p.
         |[6:14..6:15): a => a/
         |[6:16..6:17): M => a/M.
         |[6:18..6:19): P => a/M.P#
         |[6:20..6:23): Int => scala/Int#
         |[6:24..6:24):  => a/M.P#`<init>`().
         |[6:27..6:28): r => a/M.P#r.
         |[6:29..6:32): get => local3
         |[7:6..7:7): i <= b/UseM.i.
         |[7:10..7:11): a => a/
         |[7:12..7:13): M => a/M.
         |[7:14..7:15): I => a/M.I().
         |[7:16..7:19): Int => scala/Int#
         |[7:27..7:28): r => a/M.I#r.
         |[7:29..7:32): get => local4
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("#1591: refinements in parameters that scalac copies, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(copiesA, copiesB, "UseM.scala")),
      """|b/UseM. => final object UseM extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseM.d. => val method d: Int
         |  Int => scala/Int#
         |b/UseM.e. => val method e: Int
         |  Int => scala/Int#
         |b/UseM.f. => val method f: Int
         |  Int => scala/Int#
         |b/UseM.i. => val method i: Int
         |  Int => scala/Int#
         |b/UseM.p. => val method p: Int
         |  Int => scala/Int#
         |local0 => abstract method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: Int
         |  Int => scala/Int#
         |local2 => abstract method get: Int
         |  Int => scala/Int#
         |local3 => abstract method get: Int
         |  Int => scala/Int#
         |local4 => abstract method get: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseM <= b/UseM.
         |[3:6..3:7): d <= b/UseM.d.
         |[3:10..3:11): a => a/
         |[3:12..3:13): M => a/M.
         |[3:14..3:15): D => a/M.D.
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): r => a/M.D#r.
         |[3:25..3:28): get => local0
         |[4:6..4:7): e <= b/UseM.e.
         |[4:10..4:11): a => a/
         |[4:12..4:13): M => a/M.
         |[4:14..4:15): E => a/M.E.
         |[4:16..4:19): Int => scala/Int#
         |[4:27..4:28): r => a/M.E#r.
         |[4:29..4:32): get => local1
         |[5:6..5:7): f <= b/UseM.f.
         |[5:10..5:11): a => a/
         |[5:12..5:13): M => a/M.
         |[5:14..5:15): F => a/M.F.
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:24): r => a/M.F#r.
         |[5:25..5:28): get => local2
         |[6:6..6:7): p <= b/UseM.p.
         |[6:14..6:15): a => a/
         |[6:16..6:17): M => a/M.
         |[6:18..6:19): P => a/M.P#
         |[6:20..6:23): Int => scala/Int#
         |[6:24..6:24):  => a/M.P#`<init>`().
         |[6:27..6:28): r => a/M.P#r.
         |[6:29..6:32): get => local3
         |[7:6..7:7): i <= b/UseM.i.
         |[7:10..7:11): a => a/
         |[7:12..7:13): M => a/M.
         |[7:14..7:15): I => a/M.I().
         |[7:16..7:19): Int => scala/Int#
         |[7:27..7:28): r => a/M.I#r.
         |[7:29..7:32): get => local4
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // an existential in the parameter of a case class, which scalac copies into `apply`
  private val caseExistentialA = Map(
    "X.scala" ->
      """|package a
         |object X {
         |  class Box[+T](val head: T)
         |  case class W[U](r: Box[T] forSome { type T <: { def get: U } })
         |}
         |""".stripMargin,
  )

  private val caseExistentialB = Map(
    "UseX.scala" ->
      """|package b
         |import scala.language.reflectiveCalls
         |object UseX {
         |  val w = a.X.W[Int](null).r
         |  val g = a.X.W[Int](null).r.head.get
         |}
         |""".stripMargin,
  )

  test("an existential in the parameter of a case class, in the same file") {
    val code = caseExistentialA("X.scala") + caseExistentialB("UseX.scala")
      .replace("package b\n", "")
    assertNoDiff(
      locals(document(compile(Map("X.scala" -> code)), "X.scala")),
      """|local0 => abstract type T <: AnyRef { def get: U }
         |  AnyRef => scala/AnyRef#
         |  get => local1
         |  U => a/X.W#[U]
         |local1 => abstract method get: U
         |  U => a/X.W#[U]
         |local2 => synthetic val local W$1: W[U]
         |  W => a/X.W#
         |  U => a/X.W#[U]
         |""".stripMargin,
    )
  }

  test("an existential in the parameter of a case class, in another file of the run") {
    assertNoDiff(
      summary(inOneRun(caseExistentialA, caseExistentialB, "UseX.scala")),
      """|b/UseX. => final object UseX extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseX.g. => val method g: Int
         |  Int => scala/Int#
         |b/UseX.w. => val method w: Box[AnyRef { def get: U }]
         |  Box => a/X.Box#
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |  U => a/X.W#[U]
         |local0 => abstract method get: U
         |  U => a/X.W#[U]
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseX <= b/UseX.
         |[3:6..3:7): w <= b/UseX.w.
         |[3:10..3:11): a => a/
         |[3:12..3:13): X => a/X.
         |[3:14..3:15): W => a/X.W.
         |[3:16..3:19): Int => scala/Int#
         |[3:27..3:28): r => a/X.W#r.
         |[4:6..4:7): g <= b/UseX.g.
         |[4:10..4:11): a => a/
         |[4:12..4:13): X => a/X.
         |[4:14..4:15): W => a/X.W.
         |[4:16..4:19): Int => scala/Int#
         |[4:27..4:28): r => a/X.W#r.
         |[4:29..4:33): head => a/X.Box#head.
         |[4:34..4:37): get => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  test("an existential in the parameter of a case class, from a classfile") {
    assertNoDiff(
      summary(fromClassfile(caseExistentialA, caseExistentialB, "UseX.scala")),
      """|b/UseX. => final object UseX extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |b/UseX.g. => val method g: Int
         |  Int => scala/Int#
         |b/UseX.w. => val method w: Box[AnyRef { def get: Int }]
         |  Box => a/X.Box#
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |  Int => scala/Int#
         |local0 => abstract method get: Int
         |  Int => scala/Int#
         |local1 => abstract method get: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:12): scala => scala/
         |[1:13..1:21): language => scala/language.
         |[1:22..1:37): reflectiveCalls => scala/language.reflectiveCalls.
         |[2:7..2:11): UseX <= b/UseX.
         |[3:6..3:7): w <= b/UseX.w.
         |[3:10..3:11): a => a/
         |[3:12..3:13): X => a/X.
         |[3:14..3:15): W => a/X.W.
         |[3:16..3:19): Int => scala/Int#
         |[3:27..3:28): r => a/X.W#r.
         |[4:6..4:7): g <= b/UseX.g.
         |[4:10..4:11): a => a/
         |[4:12..4:13): X => a/X.
         |[4:14..4:15): W => a/X.W.
         |[4:16..4:19): Int => scala/Int#
         |[4:27..4:28): r => a/X.W#r.
         |[4:29..4:33): head => a/X.Box#head.
         |[4:34..4:37): get => local1
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // the type parameters of local classes, which scalac copies into synthetic methods
  private val localClasses =
    """|package a
       |object L {
       |  def f = {
       |    case class k[+T](t: T)
       |    k(1).copy(t = 2).t
       |  }
       |  def g = {
       |    object j
       |    case class j[A, B](a: A, b: Option[B] = None)
       |    j(1, Some("")).copy(a = 2).a
       |  }
       |  def h = {
       |    implicit class ic[T](t: T) { def me: T = t }
       |    1.me
       |  }
       |  def i = {
       |    class p[T](val t: Option[T] = None)
       |    new p[Int]().t
       |  }
       |}
       |""".stripMargin

  private val Local = """local\d+""".r

  /* the SymbolInformation whose first line includes one of `heads`, with locals numbered in the
   * order they appear, so that the synthetic members of other Scala versions do not shift them */
  private def infos(doc: s.TextDocument, heads: String*): String = {
    val kept = new StringBuilder
    var keep = false
    symbols(doc).linesIterator.foreach { line =>
      if (!line.startsWith(" ")) keep = heads.exists(line.contains)
      if (keep) kept.append(line).append('\n')
    }
    val numbers = new mutable.HashMap[String, Int]
    Local.replaceAllIn(kept.result(), m => "local" + numbers.getOrElseUpdate(m.matched, numbers.size))
  }

  test("type parameters of local classes with synthetic methods") {
    val doc = document(compile(Map("L.scala" -> localClasses)), "L.scala")
    val heads = Seq(
      "typeparam",
      "ic[",
      "copy[",
      "apply[",
      "$default$",
      "canEqual",
      "equals(",
      "toString",
      "param x$1",
    )
    assertNoDiff(
      infos(doc, heads: _*),
      if (ScalaVersion.is212)
        """|local0 => synthetic method copy[+T](val t: T): k[T]
           |  T => local1
           |  t => local2
           |  k => local3
           |local4 => synthetic method copy$default$1[+T]: T @uncheckedVariance
           |  T => local1
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local5 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
           |  x$1 => local6
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local7 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
           |  String => java/lang/String#
           |local8 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
           |  x$1 => local9
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local1 => covariant typeparam T
           |local10 => synthetic param x$1: Int
           |  Int => scala/Int#
           |local6 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local9 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local11 => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
           |  String => java/lang/String#
           |local12 => synthetic method apply[+T](val t: T): k[T]
           |  T => local1
           |  t => local2
           |  k => local3
           |local13 => synthetic method unapply[+T](x$0: k[T]): Option[T]
           |  T => local1
           |  x$0 => local14
           |  k => local3
           |  Option => scala/Option#
           |local15 => synthetic method apply[A, B](val a: A, val b: Option[B]): j[A, B]
           |  A => local16
           |  B => local17
           |  a => local18
           |  b => local19
           |  Option => scala/Option#
           |  j => local20
           |local21 => synthetic method apply$default$2[A, B]: None.type @uncheckedVariance
           |  A => local16
           |  B => local17
           |  None => scala/None.
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local22 => synthetic method unapply[A, B](x$0: j[A, B]): Option[Tuple2[A, Option[B]]]
           |  A => local16
           |  B => local17
           |  x$0 => local23
           |  j => local20
           |  Option => scala/Option#
           |  Tuple2 => scala/Tuple2#
           |local24 => synthetic method <init>$default$2[A, B]: None.type @uncheckedVariance
           |  A => local16
           |  B => local17
           |  None => scala/None.
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local16 => typeparam A
           |local17 => typeparam B
           |local25 => synthetic method copy[A, B](val a: A, val b: Option[B]): j[A, B]
           |  A => local16
           |  B => local17
           |  a => local18
           |  b => local19
           |  Option => scala/Option#
           |  j => local20
           |local26 => synthetic method copy$default$1[A, B]: A @uncheckedVariance
           |  A => local16
           |  B => local17
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local27 => synthetic method copy$default$2[A, B]: Option[B] @uncheckedVariance
           |  A => local16
           |  B => local17
           |  Option => scala/Option#
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local28 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
           |  x$1 => local29
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local30 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
           |  String => java/lang/String#
           |local31 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
           |  x$1 => local32
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local33 => synthetic param x$1: Int
           |  Int => scala/Int#
           |local29 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local32 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local34 => implicit class ic[T] extends AnyRef { +3 decls }
           |  T => local35
           |  AnyRef => scala/AnyRef#
           |local35 => typeparam T
           |local36 => synthetic implicit method ic[T](t: T): ic[T]
           |  T => local35
           |  t => local37
           |  ic => local34
           |local38 => typeparam T
           |local39 => synthetic method <init>$default$1[T]: None.type @uncheckedVariance
           |  T => local38
           |  None => scala/None.
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |""".stripMargin
      else
        """|local0 => synthetic method copy[+T](val t: T): k[T]
           |  T => local1
           |  t => local2
           |  k => local3
           |local4 => synthetic method copy$default$1[+T]: T @uncheckedVariance
           |  T => local1
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local5 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
           |  x$1 => local6
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local7 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
           |  String => java/lang/String#
           |local8 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
           |  x$1 => local9
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local1 => covariant typeparam T
           |local10 => synthetic param x$1: Int
           |  Int => scala/Int#
           |local6 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local11 => synthetic param x$1: Int
           |  Int => scala/Int#
           |local9 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local12 => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
           |  String => java/lang/String#
           |local13 => synthetic method apply[+T](val t: T): k[T]
           |  T => local1
           |  t => local2
           |  k => local3
           |local14 => synthetic method unapply[+T](x$0: k[T]): Option[T]
           |  T => local1
           |  x$0 => local15
           |  k => local3
           |  Option => scala/Option#
           |local16 => synthetic method apply[A, B](val a: A, val b: Option[B]): j[A, B]
           |  A => local17
           |  B => local18
           |  a => local19
           |  b => local20
           |  Option => scala/Option#
           |  j => local21
           |local22 => synthetic method apply$default$2[A, B]: None.type @uncheckedVariance
           |  A => local17
           |  B => local18
           |  None => scala/None.
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local23 => synthetic method unapply[A, B](x$0: j[A, B]): Option[Tuple2[A, Option[B]]]
           |  A => local17
           |  B => local18
           |  x$0 => local24
           |  j => local21
           |  Option => scala/Option#
           |  Tuple2 => scala/Tuple2#
           |local25 => synthetic method <init>$default$2[A, B]: None.type @uncheckedVariance
           |  A => local17
           |  B => local18
           |  None => scala/None.
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local17 => typeparam A
           |local18 => typeparam B
           |local26 => synthetic method copy[A, B](val a: A, val b: Option[B]): j[A, B]
           |  A => local17
           |  B => local18
           |  a => local19
           |  b => local20
           |  Option => scala/Option#
           |  j => local21
           |local27 => synthetic method copy$default$1[A, B]: A @uncheckedVariance
           |  A => local17
           |  B => local18
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local28 => synthetic method copy$default$2[A, B]: Option[B] @uncheckedVariance
           |  A => local17
           |  B => local18
           |  Option => scala/Option#
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |local29 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
           |  x$1 => local30
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local31 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
           |  String => java/lang/String#
           |local32 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
           |  x$1 => local33
           |  Any => scala/Any#
           |  Boolean => scala/Boolean#
           |local34 => synthetic param x$1: Int
           |  Int => scala/Int#
           |local30 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local35 => synthetic param x$1: Int
           |  Int => scala/Int#
           |local33 => synthetic param x$1: Any
           |  Any => scala/Any#
           |local36 => implicit class ic[T] extends AnyRef { +3 decls }
           |  T => local37
           |  AnyRef => scala/AnyRef#
           |local37 => typeparam T
           |local38 => synthetic implicit method ic[T](t: T): ic[T]
           |  T => local37
           |  t => local39
           |  ic => local36
           |local40 => typeparam T
           |local41 => synthetic method <init>$default$1[T]: None.type @uncheckedVariance
           |  T => local40
           |  None => scala/None.
           |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
           |""".stripMargin,
    )
  }

  // members that scalac adds to local classes, at the offset of the class
  private val localSynthetics =
    """|package a
       |object S {
       |  def f = { case class k(t: Int); val v = k(1); v.canEqual(v) && v.equals(v) }
       |  def o = { case object co; co.canEqual(co) }
       |  def e = { case class ke(); ke().canEqual(1) }
       |  def m = { case class km(a: Int)(b: Int); km(1)(2).canEqual(1) }
       |}
       |""".stripMargin

  test("synthetic members of local case classes and a local case object") {
    val doc = document(compile(Map("S.scala" -> localSynthetics)), "S.scala")
    assertNoDiff(
      infos(doc, "productElement(", "canEqual", "equals(", "toString"),
      """|local0 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local1
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local2 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local3
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local4 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local5 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local6
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local7 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local9
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local11
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local14
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local15 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local17 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local18 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local20 => synthetic final method toString(): String <: scala/Function0#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local21 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local22
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local23 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local24
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local25 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local26 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local27
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local28 => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |""".stripMargin,
    )
  }

  // definitions that scalac adds outside case classes
  private val localDefinitions =
    """|package a
       |import scala.beans.BeanProperty
       |object D {
       |  def b = { class kb { @BeanProperty var x: Int = 0 }; val v = new kb; v.x = 1; v.setX(2) }
       |  def pf: PartialFunction[Int, Int] = { case x if x > 0 => x }
       |}
       |""".stripMargin
  private lazy val localDefinitionsDoc =
    document(compile(Map("D.scala" -> localDefinitions)), "D.scala")

  test("the setters of a bean property in a local class") {
    assertNoDiff(
      infos(localDefinitionsDoc, "x_=", "param x$1"),
      """|local0 => var method x_=(x$1: Int): Unit
         |  x$1 => local1
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |local1 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local2 => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin,
    )
  }

  test("a partial function literal") {
    assertNoDiff(
      infos(localDefinitionsDoc, "applyOrElse", "isDefinedAt"),
      """|local0 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local1
         |  Int => scala/Int#
         |  B1 => local2
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local5 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local6
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |""".stripMargin,
    )
  }

  // declarations and the definitions that scalac derives from them
  private val localCopies =
    """|package a
       |object C {
       |  def p = { class kp(val a: Int); new kp(a = 1).a }
       |  def fc = for { x <- List(1) if x > 0; (y, z) <- List((x, x)); w = y } yield x + w
       |}
       |""".stripMargin
  private lazy val localCopiesDoc = document(compile(Map("C.scala" -> localCopies)), "C.scala")

  test("a val parameter of a local class")(assertNoDiff(
    infos(localCopiesDoc, "class kp", "val param a:"),
    """|local0 => class kp extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param a: Int
       |  Int => scala/Int#
       |""".stripMargin,
  ))

  test("references to a val parameter of a local class and to binders of a for comprehension") {
    assertNoDiff(
      occurrences(localCopiesDoc).linesIterator.filter(_.contains("local")).mkString("\n"),
      """|[2:18..2:20): kp <= local0
         |[2:20..2:20):  <= local2
         |[2:25..2:26): a <= local1
         |[2:38..2:40): kp => local0
         |[2:40..2:40):  => local2
         |[2:41..2:42): a => local1
         |[2:48..2:49): a => local1
         |[3:17..3:18): x <= local3
         |[3:33..3:34): x => local3
         |[3:41..3:42): y <= local4
         |[3:44..3:45): z <= local5
         |[3:56..3:57): x => local3
         |[3:59..3:60): x => local3
         |[3:64..3:65): w <= local6
         |[3:68..3:69): y => local4
         |[3:78..3:79): x => local3
         |[3:82..3:83): w => local6
         |""".stripMargin,
    )
  }

  // binders of generators that scalac also makes parameters of closures
  private val forBinders =
    """|package a
       |object F {
       |  def f = for { x <- Option(1); _ = x } ()
       |  def g = for (y <- Option(1) if y > 0) yield y
       |  def h = for { z <- Option(1); w = z; if w > 0 } yield (z, w)
       |}
       |""".stripMargin

  test("binders of for comprehensions") {
    assertNoDiff(
      summary(document(compile(Map("F.scala" -> forBinders)), "F.scala")),
      """|a/F. => final object F extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |a/F.f(). => method f: Unit
         |  Unit => scala/Unit#
         |a/F.g(). => method g: Option[Int]
         |  Option => scala/Option#
         |  Int => scala/Int#
         |a/F.h(). => method h: Option[Tuple2[Int, Int]]
         |  Option => scala/Option#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local1 => val local x$1: Int
         |  Int => scala/Int#
         |local2 => param y: Int
         |  Int => scala/Int#
         |local3 => val local z: Int
         |  Int => scala/Int#
         |local4 => val local w: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:8): F <= a/F.
         |[2:6..2:7): f <= a/F.f().
         |[2:16..2:17): x <= local0
         |[2:21..2:27): Option => scala/Option.
         |[2:32..2:33): _ <= local1
         |[2:36..2:37): x => local0
         |[3:6..3:7): g <= a/F.g().
         |[3:15..3:16): y <= local2
         |[3:20..3:26): Option => scala/Option.
         |[3:33..3:34): y => local2
         |[3:35..3:36): > => scala/Int#`>`(+3).
         |[3:46..3:47): y => local2
         |[4:6..4:7): h <= a/F.h().
         |[4:16..4:17): z <= local3
         |[4:21..4:27): Option => scala/Option.
         |[4:32..4:33): w <= local4
         |[4:36..4:37): z => local3
         |[4:42..4:43): w => local4
         |[4:44..4:45): > => scala/Int#`>`(+3).
         |[4:57..4:58): z => local3
         |[4:60..4:61): w => local4
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // a local case class that escapes its method in a refinement
  private val escapingCaseClass = Map(
    "E.scala" ->
      """|package a
         |object E {
         |  def esc = { case class ks(t: Int); ks(1) }
         |}
         |""".stripMargin,
  )

  private val escapingCaseClassUse = Map(
    "U.scala" ->
      """|package b
         |object U {
         |  val v = a.E.esc
         |  val c = v.copy(t = 2)
         |  val e = v.canEqual(v)
         |}
         |""".stripMargin,
  )

  private val escapingHeads =
    Seq("method copy(", "method canEqual", "method equals(", "method toString")

  test("a local case class that escapes its method, in another file of the run") {
    val doc = inOneRun(escapingCaseClass, escapingCaseClassUse, "U.scala")
    assertNoDiff(
      infos(doc, escapingHeads: _*),
      """|local0 => synthetic method copy(val t: Int): ks
         |  t => local1
         |  Int => scala/Int#
         |  ks => local2
         |local3 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local4
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local5 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local6 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local7
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |""".stripMargin,
    )
  }

  test("a local case class that escapes its method, from a classfile") {
    val doc = fromClassfile(escapingCaseClass, escapingCaseClassUse, "U.scala")
    assertNoDiff(
      infos(doc, escapingHeads: _*),
      """|local0 => synthetic method copy(t: Int): ks
         |  t => local1
         |  Int => scala/Int#
         |  ks => local2
         |""".stripMargin,
    )
  }

  test("references to the binder of a partial function literal") {
    assertNoDiff(
      occurrences(localDefinitionsDoc).linesIterator.filter(_.contains("local")).mkString("\n"),
      """|[3:18..3:20): kb <= local0
         |[3:21..3:21):  <= local1
         |[3:41..3:42): x <= local2
         |[3:59..3:60): v <= local8
         |[3:67..3:69): kb => local0
         |[3:69..3:69):  => local1
         |[3:71..3:72): v => local8
         |[3:73..3:74): x => local3
         |[3:80..3:81): v => local8
         |[3:82..3:86): setX => local5
         |[4:38..4:38):  <= local9
         |[4:45..4:46): x <= local16
         |[4:50..4:51): x => local16
         |[4:59..4:60): x => local16
         |""".stripMargin,
    )
  }

  test("references to the parameters of the copy of a local case class") {
    val doc = document(compile(Map("L.scala" -> localClasses)), "L.scala")
    // number the locals in the order they appear, as `infos` does
    val numbers = new mutable.HashMap[String, Int]
    val lines = occurrences(doc).linesIterator.filter(_.contains("local")).mkString("\n")
    assertNoDiff(
      Local.replaceAllIn(lines, m => "local" + numbers.getOrElseUpdate(m.matched, numbers.size)),
      """|[3:15..3:16): k <= local0
         |[3:18..3:19): T <= local1
         |[3:20..3:20):  <= local2
         |[3:21..3:22): t <= local3
         |[3:24..3:25): T => local1
         |[4:4..4:5): k => local4
         |[4:9..4:13): copy => local5
         |[4:14..4:15): t => local3
         |[4:21..4:22): t => local3
         |[7:11..7:12): j <= local6
         |[8:15..8:16): j <= local7
         |[8:17..8:18): A <= local8
         |[8:20..8:21): B <= local9
         |[8:22..8:22):  <= local10
         |[8:23..8:24): a <= local11
         |[8:26..8:27): A => local8
         |[8:29..8:30): b <= local12
         |[8:39..8:40): B => local9
         |[9:4..9:5): j => local6
         |[9:19..9:23): copy => local13
         |[9:24..9:25): a => local11
         |[9:31..9:32): a => local11
         |[12:19..12:21): ic <= local14
         |[12:22..12:23): T <= local15
         |[12:24..12:24):  <= local16
         |[12:25..12:26): t <= local17
         |[12:28..12:29): T => local15
         |[12:37..12:39): me <= local18
         |[12:41..12:42): T => local15
         |[12:45..12:46): t => local17
         |[13:6..13:8): me => local18
         |[16:10..16:11): p <= local19
         |[16:12..16:13): T <= local20
         |[16:14..16:14):  <= local21
         |[16:19..16:20): t <= local22
         |[16:29..16:30): T => local20
         |[17:8..17:9): p => local19
         |[17:14..17:14):  => local21
         |[17:17..17:18): t => local22
         |""".stripMargin,
    )
  }

  // a local method whose default getter scalac derives with its own type parameter
  private val localDefault =
    """|package a
       |object DA {
       |  def f = { def g[T](a: T, b: T = null.asInstanceOf[T]): T = b; g(1) }
       |}
       |""".stripMargin

  test("a local method with a default argument and a type parameter") {
    assertNoDiff(
      summary(document(compile(Map("DA.scala" -> localDefault)), "DA.scala")),
      """|a/DA. => final object DA extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |a/DA.f(). => method f: Int
         |  Int => scala/Int#
         |local0 => method g[T](a: T, b: T): T
         |  T => local3
         |  a => local1
         |  b => local2
         |local1 => param a: T
         |  T => local3
         |local2 => default param b: T
         |  T => local3
         |local3 => typeparam T
         |local4 => synthetic method g$default$2[T]: T @uncheckedVariance
         |  T => local3
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:9): DA <= a/DA.
         |[2:6..2:7): f <= a/DA.f().
         |[2:16..2:17): g <= local0
         |[2:18..2:19): T <= local3
         |[2:21..2:22): a <= local1
         |[2:24..2:25): T => local3
         |[2:27..2:28): b <= local2
         |[2:30..2:31): T => local3
         |[2:39..2:51): asInstanceOf => scala/Any#asInstanceOf().
         |[2:52..2:53): T => local3
         |[2:57..2:58): T => local3
         |[2:61..2:62): b => local2
         |[2:64..2:65): g => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // a refinement in the bound of a type parameter of a method
  private val boundA = Map(
    "TB.scala" ->
      """|package a
         |object TB {
         |  def f[A <: { def x: Int }](a: A): Int = a.x
         |  def g[A <: { def x: Int }](a: A): Int = a.x + f(a)
         |}
         |""".stripMargin,
  )

  test("#1591: a refinement in the bound of a type parameter, in the same file") {
    assertNoDiff(
      summary(document(compile(boundA), "TB.scala")),
      """|a/TB. => final object TB extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |a/TB.f(). => method f[A <: AnyRef { def x: Int }](a: A): Int
         |  A => a/TB.f().[A]
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |  a => a/TB.f().(a)
         |a/TB.f().(a) => param a: A
         |  A => a/TB.f().[A]
         |a/TB.f().[A] => typeparam A <: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local0
         |  Int => scala/Int#
         |a/TB.g(). => method g[A <: AnyRef { def x: Int }](a: A): Int
         |  A => a/TB.g().[A]
         |  AnyRef => scala/AnyRef#
         |  x => local1
         |  Int => scala/Int#
         |  a => a/TB.g().(a)
         |a/TB.g().(a) => param a: A
         |  A => a/TB.g().[A]
         |a/TB.g().[A] => typeparam A <: AnyRef { def x: Int }
         |  AnyRef => scala/AnyRef#
         |  x => local1
         |  Int => scala/Int#
         |local0 => abstract method x: Int
         |  Int => scala/Int#
         |local1 => abstract method x: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): a <= a/
         |[1:7..1:9): TB <= a/TB.
         |[2:6..2:7): f <= a/TB.f().
         |[2:8..2:9): A <= a/TB.f().[A]
         |[2:19..2:20): x <= local0
         |[2:22..2:25): Int => scala/Int#
         |[2:29..2:30): a <= a/TB.f().(a)
         |[2:32..2:33): A => a/TB.f().[A]
         |[2:36..2:39): Int => scala/Int#
         |[2:42..2:43): a => a/TB.f().(a)
         |[2:44..2:45): x => local0
         |[3:6..3:7): g <= a/TB.g().
         |[3:8..3:9): A <= a/TB.g().[A]
         |[3:19..3:20): x <= local1
         |[3:22..3:25): Int => scala/Int#
         |[3:29..3:30): a <= a/TB.g().(a)
         |[3:32..3:33): A => a/TB.g().[A]
         |[3:36..3:39): Int => scala/Int#
         |[3:42..3:43): a => a/TB.g().(a)
         |[3:44..3:45): x => local1
         |[3:46..3:47): + => scala/Int#`+`(+4).
         |[3:48..3:49): f => a/TB.f().
         |[3:50..3:51): a => a/TB.g().(a)
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // a macro that expands its argument twice
  private val twiceA = Map {
    "T.scala" ->
      """|package a
         |import scala.language.experimental.macros
         |import scala.reflect.macros.whitebox
         |object T {
         |  def twice(a: Any): Any = macro impl
         |  def impl(c: whitebox.Context)(a: c.Tree): c.Tree = {
         |    import c.universe._
         |    q"(${c.untypecheck(a)}, ${c.untypecheck(a)})"
         |  }
         |}
         |""".stripMargin
  }

  test("a macro that expands its argument twice") {
    val b = Map(
      "U.scala" ->
        """|package b
           |object U {
           |  val t = a.T.twice { val x = 1; x }
           |}
           |""".stripMargin,
    )
    assertNoDiff(
      summary(fromClassfile(twiceA, b, "U.scala")),
      """|b/U. => final object U extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |b/U.t. => val method t: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |----
         |[0:8..0:9): b <= b/
         |[1:7..1:8): U <= b/U.
         |[2:6..2:7): t <= b/U.t.
         |[2:10..2:11): a => a/
         |[2:12..2:13): T => a/T.
         |[2:14..2:19): twice => a/T.twice().
         |[2:26..2:27): x <= local0
         |[2:33..2:34): x => local0
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

  // the errors of a run that compiles `sources`, with the plugin
  private def errors(sources: Map[String, String]): List[String] = reporting
    .compile(sources, Files.createTempDirectory("issue1591_"))
    .map(info => s"${info.pos.source.file.name}:${info.pos.line}:${info.pos.column}: ${info.msg}")

  private val outsideBounds =
    """|object A {
       |  type F[T <: String] = List[T]
       |  def f = { val x: F[Int] = ???; x }
       |  val r: { def y: Int } = ???
       |}
       |""".stripMargin

  // each error once, though the document and the index both run the deferred check of the bounds
  private val outsideBoundsErrors = List(
    "A.scala:3:7: type arguments [Int] do not conform to type F's type parameter bounds [T <: String]",
    "A.scala:3:20: type arguments [Int] do not conform to type F's type parameter bounds [T <: String]",
  )

  test("a type alias applied outside its bounds, in the same file")(
    assertEquals(errors(Map("A.scala" -> outsideBounds)), outsideBoundsErrors),
  )

  test("a type alias applied outside its bounds, in another file of the run") {
    val b = "object B { val y = A.r.y }\n"
    assertEquals(errors(Map("A.scala" -> outsideBounds, "B.scala" -> b)), outsideBoundsErrors)
  }

  test("a package that an earlier run created") {
    errors(Map("A.scala" -> "package zz\nobject Z\n"))
    assertEquals(
      errors(Map("B.scala" -> "object B { val z = zz.Z }\n")),
      List("B.scala:1:20: not found: value zz"),
    )
  }

  test("an existential of a classfile, outside a document and then in a document") {
    val typed = new Typed("object O { val v = Option(Class.forName(\"C\")) }")
    typed.ops.config = typed.ops.config.copy(synthetics = BinaryMode.On)
    val before = typed.existentials
    assertNoDiff(
      section(typed.document, "Synthetics"),
      """|[0:19..0:25) => *.apply[Class[?0]]
         |  apply => scala/Option.apply().
         |  Class => java/lang/Class#
         |  ?0 => local0
         |""".stripMargin,
    )
    assertEquals(
      (before, typed.existentials),
      (List("local0", "local1", "local0"), List("local1", "local0", "local0")),
    )
  }

  test("#1591: locals in an annotated type, a refinement and a bounded alias, in the same file") {
    val code =
      """|package p
         |class ann(f: Int => Int) extends scala.annotation.StaticAnnotation
         |object O {
         |  type F[T] = { def get: T }
         |  type G[T <: AnyRef] = { def get: T }
         |  def f = {
         |    val x: Int @ann({ val q = 1; (p: Int) => p + q }) = 1
         |    val r: AnyRef { def y(a: Int): Int; type U <: { def w: Int } } = ???
         |    val v: F[Int] = ???
         |    val g: G[String] = ???
         |    def h[K](k: K): K { def z: Int } = ???
         |    (x, r.y(1), v.get, g.get, h(new { def z = 1 }).z, (??? : r.U).w)
         |  }
         |}
         |""".stripMargin
    assertNoDiff(
      summary(document(compile(Map("P.scala" -> code)), "P.scala")),
      """|local0 => abstract method get: T
         |  T => p/O.F#[T]
         |local1 => abstract method get: T
         |  T => p/O.G#[T]
         |local2 => val local x: Int @ann
         |  Int => scala/Int#
         |  ann => p/ann#
         |local3 => val local q: Int
         |  Int => scala/Int#
         |local4 => param p: Int
         |  Int => scala/Int#
         |local5 => val local r: AnyRef { def y(a: Int): Int; type U <: AnyRef { def w: Int } }
         |  AnyRef => scala/AnyRef#
         |  y => local6
         |  a => local7
         |  Int => scala/Int#
         |  U => local8
         |  w => local9
         |local6 => abstract method y(a: Int): Int
         |  a => local7
         |  Int => scala/Int#
         |local7 => param a: Int
         |  Int => scala/Int#
         |local8 => abstract type U <: AnyRef { def w: Int }
         |  AnyRef => scala/AnyRef#
         |  w => local9
         |  Int => scala/Int#
         |local9 => abstract method w: Int
         |  Int => scala/Int#
         |local10 => val local v: F[Int]
         |  F => p/O.F#
         |  Int => scala/Int#
         |local11 => val local g: G[String]
         |  G => p/O.G#
         |  String => scala/Predef.String#
         |local12 => method h[K](k: K): K { def z: Int }
         |  K => local14
         |  k => local13
         |  z => local15
         |  Int => scala/Int#
         |local13 => param k: K
         |  K => local14
         |local14 => typeparam K
         |local15 => abstract method z: Int
         |  Int => scala/Int#
         |local16 => method z: Int
         |  Int => scala/Int#
         |local17 => final class $anon extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |p/O. => final object O extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |p/O.F# => type F[T] = AnyRef { def get: T }
         |  T => p/O.F#[T]
         |  AnyRef => scala/AnyRef#
         |  get => local0
         |p/O.F#[T] => typeparam T
         |p/O.G# => type G[T <: AnyRef] = AnyRef { def get: T }
         |  T => p/O.G#[T]
         |  AnyRef => scala/AnyRef#
         |  get => local1
         |p/O.G#[T] => typeparam T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |p/O.f(). => method f: Tuple6[Int, Int, Int, String, Int, Int]
         |  Tuple6 => scala/Tuple6#
         |  Int => scala/Int#
         |  String => scala/Predef.String#
         |p/ann# => class ann extends Annotation with StaticAnnotation { +2 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |p/ann#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
         |  f => p/ann#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |p/ann#`<init>`().(f) => param f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |p/ann#f. => private[this] val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |----
         |[0:8..0:9): p <= p/
         |[1:6..1:9): ann <= p/ann#
         |[1:9..1:9):  <= p/ann#`<init>`().
         |[1:10..1:11): f <= p/ann#`<init>`().(f)
         |[1:10..1:11): f <= p/ann#f.
         |[1:13..1:16): Int => scala/Int#
         |[1:20..1:23): Int => scala/Int#
         |[1:33..1:38): scala => scala/
         |[1:39..1:49): annotation => scala/annotation/
         |[1:50..1:66): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:66..1:66):  => scala/annotation/Annotation#`<init>`().
         |[2:7..2:8): O <= p/O.
         |[3:7..3:8): F <= p/O.F#
         |[3:9..3:10): T <= p/O.F#[T]
         |[3:20..3:23): get <= local0
         |[3:25..3:26): T => p/O.F#[T]
         |[4:7..4:8): G <= p/O.G#
         |[4:9..4:10): T <= p/O.G#[T]
         |[4:14..4:20): AnyRef => scala/AnyRef#
         |[4:30..4:33): get <= local1
         |[4:35..4:36): T => p/O.G#[T]
         |[5:6..5:7): f <= p/O.f().
         |[6:8..6:9): x <= local2
         |[6:11..6:14): Int => scala/Int#
         |[6:16..6:19): ann => p/ann#
         |[6:19..6:19):  => p/ann#`<init>`().
         |[6:26..6:27): q <= local3
         |[6:34..6:35): p <= local4
         |[6:37..6:40): Int => scala/Int#
         |[6:45..6:46): p => local4
         |[6:47..6:48): + => scala/Int#`+`(+4).
         |[6:49..6:50): q => local3
         |[7:8..7:9): r <= local5
         |[7:11..7:17): AnyRef => scala/AnyRef#
         |[7:24..7:25): y <= local6
         |[7:26..7:27): a <= local7
         |[7:29..7:32): Int => scala/Int#
         |[7:35..7:38): Int => scala/Int#
         |[7:45..7:46): U <= local8
         |[7:56..7:57): w <= local9
         |[7:59..7:62): Int => scala/Int#
         |[7:69..7:72): ??? => scala/Predef.`???`().
         |[8:8..8:9): v <= local10
         |[8:11..8:12): F => p/O.F#
         |[8:13..8:16): Int => scala/Int#
         |[8:20..8:23): ??? => scala/Predef.`???`().
         |[9:8..9:9): g <= local11
         |[9:11..9:12): G => p/O.G#
         |[9:13..9:19): String => scala/Predef.String#
         |[9:23..9:26): ??? => scala/Predef.`???`().
         |[10:8..10:9): h <= local12
         |[10:10..10:11): K <= local14
         |[10:13..10:14): k <= local13
         |[10:16..10:17): K => local14
         |[10:20..10:21): K => local14
         |[10:28..10:29): z <= local15
         |[10:31..10:34): Int => scala/Int#
         |[10:39..10:42): ??? => scala/Predef.`???`().
         |[11:5..11:6): x => local2
         |[11:8..11:9): r => local5
         |[11:10..11:11): y => local6
         |[11:16..11:17): v => local10
         |[11:18..11:21): get => local0
         |[11:23..11:24): g => local11
         |[11:25..11:28): get => local1
         |[11:30..11:31): h => local12
         |[11:36..11:36):  <= local17
         |[11:42..11:43): z <= local16
         |[11:51..11:52): z => local16
         |[11:55..11:58): ??? => scala/Predef.`???`().
         |[11:61..11:62): r => local5
         |[11:63..11:64): U => local8
         |[11:66..11:67): w => local9
         |----
         |collisions List()
         |dangling List()
         |""".stripMargin,
    )
  }

}
