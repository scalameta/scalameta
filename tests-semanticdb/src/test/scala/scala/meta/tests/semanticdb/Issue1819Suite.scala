package scala.meta.tests.semanticdb

import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.semanticdb.{Print, SymbolInformation}
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.metap.Format
import scala.meta.tests.metacp.Library

import java.nio.file.Files

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

// https://github.com/scalameta/scalameta/issues/1819
class Issue1819Suite extends SemanticdbSuite {

  private val code =
    """|package a
       |trait Empty
       |trait Abstract { def m: Int }
       |trait Init { val x = 1; println(x) }
       |class C1 extends Init
       |class C2 extends AnyRef with Init with Empty
       |object O { val anon = new Init {}; val anon2 = new Empty {} }
       |trait Stat { println(1) }
       |trait Lazy { lazy val l = 1 }
       |trait Defs { def d = 1 }
       |trait Decls { type T; val a: Int; def b: Int }
       |trait Sub extends Init { val y = 2 }
       |object L { def m = { trait Loc { val z = 1 }; 1 } }
       |""".stripMargin

  symbols(
    code,
    """|a/Abstract# => trait Abstract extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |a/Abstract#m(). => abstract method m: Int
       |  Int => scala/Int#
       |a/C1# => class C1 extends AnyRef with Init { +1 decls }
       |  AnyRef => scala/AnyRef#
       |  Init => a/Init#
       |a/C1#`<init>`(). => primary ctor <init>()
       |a/C2# => class C2 extends AnyRef with Init with Empty { +1 decls }
       |  AnyRef => scala/AnyRef#
       |  Init => a/Init#
       |  Empty => a/Empty#
       |a/C2#`<init>`(). => primary ctor <init>()
       |a/Decls# => trait Decls extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |a/Decls#T# => abstract type T
       |a/Decls#a. => abstract val method a: Int
       |  Int => scala/Int#
       |a/Decls#b(). => abstract method b: Int
       |  Int => scala/Int#
       |a/Defs# => trait Defs extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |a/Defs#`<init>`(). => primary ctor <init>()
       |a/Defs#d(). => method d: Int
       |  Int => scala/Int#
       |a/Empty# => trait Empty extends AnyRef
       |  AnyRef => scala/AnyRef#
       |a/Init# => trait Init extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |a/Init#`<init>`(). => primary ctor <init>()
       |a/Init#x. => val method x: Int
       |  Int => scala/Int#
       |a/L. => final object L extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |a/L.m(). => method m: Int
       |  Int => scala/Int#
       |a/Lazy# => trait Lazy extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |a/Lazy#`<init>`(). => primary ctor <init>()
       |a/Lazy#l. => lazy val method l: Int
       |  Int => scala/Int#
       |a/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |a/O.anon. => val method anon: AnyRef with Init {}
       |  AnyRef => scala/AnyRef#
       |  Init => a/Init#
       |a/O.anon2. => val method anon2: AnyRef with Empty {}
       |  AnyRef => scala/AnyRef#
       |  Empty => a/Empty#
       |a/Stat# => trait Stat extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |a/Stat#`<init>`(). => primary ctor <init>()
       |a/Sub# => trait Sub extends AnyRef with Init { +2 decls }
       |  AnyRef => scala/AnyRef#
       |  Init => a/Init#
       |a/Sub#`<init>`(). => primary ctor <init>()
       |a/Sub#y. => val method y: Int
       |  Int => scala/Int#
       |local0 => final class $anon extends AnyRef with Init
       |  AnyRef => scala/AnyRef#
       |  Init => a/Init#
       |local1 => final class $anon extends AnyRef with Empty
       |  AnyRef => scala/AnyRef#
       |  Empty => a/Empty#
       |local2 => trait Loc extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local3 => primary ctor <init>()
       |local4 => val method z: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    code,
    """|[0:8..0:9): a <= a/
       |[1:6..1:11): Empty <= a/Empty#
       |[2:6..2:14): Abstract <= a/Abstract#
       |[2:21..2:22): m <= a/Abstract#m().
       |[2:24..2:27): Int => scala/Int#
       |[3:6..3:10): Init <= a/Init#
       |[3:11..3:11):  <= a/Init#`<init>`().
       |[3:17..3:18): x <= a/Init#x.
       |[3:24..3:31): println => scala/Predef.println(+1).
       |[3:32..3:33): x => a/Init#x.
       |[4:6..4:8): C1 <= a/C1#
       |[4:9..4:9):  <= a/C1#`<init>`().
       |[4:17..4:21): Init => a/Init#
       |[4:21..4:21):  => java/lang/Object#`<init>`().
       |[5:6..5:8): C2 <= a/C2#
       |[5:9..5:9):  <= a/C2#`<init>`().
       |[5:17..5:23): AnyRef => scala/AnyRef#
       |[5:23..5:23):  => java/lang/Object#`<init>`().
       |[5:29..5:33): Init => a/Init#
       |[5:39..5:44): Empty => a/Empty#
       |[6:7..6:8): O <= a/O.
       |[6:15..6:19): anon <= a/O.anon.
       |[6:26..6:26):  <= local0
       |[6:26..6:30): Init => a/Init#
       |[6:30..6:30):  => java/lang/Object#`<init>`().
       |[6:39..6:44): anon2 <= a/O.anon2.
       |[6:51..6:51):  <= local1
       |[6:51..6:56): Empty => a/Empty#
       |[6:56..6:56):  => java/lang/Object#`<init>`().
       |[7:6..7:10): Stat <= a/Stat#
       |[7:11..7:11):  <= a/Stat#`<init>`().
       |[7:13..7:20): println => scala/Predef.println(+1).
       |[8:6..8:10): Lazy <= a/Lazy#
       |[8:11..8:11):  <= a/Lazy#`<init>`().
       |[8:22..8:23): l <= a/Lazy#l.
       |[9:6..9:10): Defs <= a/Defs#
       |[9:11..9:11):  <= a/Defs#`<init>`().
       |[9:17..9:18): d <= a/Defs#d().
       |[10:6..10:11): Decls <= a/Decls#
       |[10:19..10:20): T <= a/Decls#T#
       |[10:26..10:27): a <= a/Decls#a.
       |[10:29..10:32): Int => scala/Int#
       |[10:38..10:39): b <= a/Decls#b().
       |[10:41..10:44): Int => scala/Int#
       |[11:6..11:9): Sub <= a/Sub#
       |[11:10..11:10):  <= a/Sub#`<init>`().
       |[11:18..11:22): Init => a/Init#
       |[11:29..11:30): y <= a/Sub#y.
       |[12:7..12:8): L <= a/L.
       |[12:15..12:16): m <= a/L.m().
       |[12:27..12:30): Loc <= local2
       |[12:31..12:31):  <= local3
       |[12:37..12:38): z <= local4
       |""".stripMargin,
  )

  private val (scalac, classdir) = {
    val out = Files.createTempDirectory("issue1819_")
    out.toFile.deleteOnExit()
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.outputDirs.setSingleOutput(out.toString)
    val reporter = new StoreReporter()
    val g = new Global(settings, reporter)
    new g.Run().compileSources(List(new BatchSourceFile("Issue1819.scala", code)))
    assertEquals(reporter.infos.toList.map(_.msg), Nil)
    (g, out)
  }

  private val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++ Classpath(AbsolutePath(classdir)),
    includeJdk = true,
  )

  private val printerSymtab = new PrinterSymtab {
    def info(symbol: String): Option[SymbolInformation] = symtab.info(symbol)
  }

  private def load(symbol: String): Option[String] = symtab.info(symbol)
    .map(Print.info(Format.Compact, _, printerSymtab))

  private def decls(symbol: String): List[String] = symtab.info(symbol).toList
    .flatMap(_.signature match {
      case sig: s.ClassSignature => sig.declarations.toList.flatMap(_.symlinks)
      case _ => Nil
    })

  private def scalacDecls(cls: String): List[String] = {
    import scalac._
    exitingTyper(rootMirror.getRequiredClass(cls).info.decls.toList.map(_.name.toString))
  }

  private val traits = List("Empty", "Abstract", "Init", "Stat", "Lazy", "Defs", "Decls", "Sub")

  test("scalac declarations of traits")(assertEquals(
    traits.map(t => t -> scalacDecls("a." + t)),
    List(
      "Empty" -> Nil,
      "Abstract" -> List("m"),
      "Init" -> List("$init$", "x"),
      "Stat" -> List("$init$"),
      "Lazy" -> List("$init$", "l"),
      "Defs" -> List("$init$", "d"),
      "Decls" -> List("T", "a", "b"),
      "Sub" -> List("$init$", "y"),
    ),
  ))

  test("metacp declarations of traits") {
    val expected = List(
      "Empty" -> Nil,
      "Abstract" -> List("a/Abstract#m()."),
      "Init" -> List("a/Init#`<init>`().", "a/Init#x."),
      "Stat" -> List("a/Stat#`<init>`()."),
      "Lazy" -> List("a/Lazy#`<init>`().", "a/Lazy#l."),
      "Defs" -> List("a/Defs#`<init>`().", "a/Defs#d()."),
      "Decls" -> List("a/Decls#T#", "a/Decls#a.", "a/Decls#b()."),
      "Sub" -> List("a/Sub#`<init>`().", "a/Sub#y."),
    )
    assertEquals(traits.map(t => t -> decls(s"a/$t#")), expected)
  }

  test("metacp constructors of traits and a class") {
    val expected = List(
      "Empty" -> None,
      "Abstract" -> None,
      "Init" -> Some("a/Init#`<init>`(). => primary ctor <init>()"),
      "Stat" -> Some("a/Stat#`<init>`(). => primary ctor <init>()"),
      "Lazy" -> Some("a/Lazy#`<init>`(). => primary ctor <init>()"),
      "Defs" -> Some("a/Defs#`<init>`(). => primary ctor <init>()"),
      "Decls" -> None,
      "Sub" -> Some("a/Sub#`<init>`(). => primary ctor <init>()"),
      "C1" -> Some("a/C1#`<init>`(). => primary ctor <init>()"),
    )
    assertEquals((traits :+ "C1").map(t => t -> load(s"a/$t#`<init>`().")), expected)
  }

}
