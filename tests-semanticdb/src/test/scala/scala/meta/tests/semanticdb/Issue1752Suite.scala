package scala.meta.tests.semanticdb

import scala.meta.cli.{Metacp, Reporter}
import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.symtab.{GlobalSymbolTable, SymbolTable}
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.metacp.Settings
import scala.meta.metap.Format
import scala.meta.tests.BuildInfo
import scala.meta.tests.metacp.{Jars, Library}

import java.nio.file.Files

// https://github.com/scalameta/scalameta/issues/1752
class Issue1752Suite extends SemanticdbSuite {

  private val symtab = GlobalSymbolTable(Library.scalaLibrary.classpath(), includeJdk = true)

  private val printerSymtab = new PrinterSymtab {
    def info(symbol: String): Option[s.SymbolInformation] = symtab.info(symbol)
  }

  private def load(symbol: String): Option[String] = load(symtab, symbol)

  private def load(symtab: SymbolTable, symbol: String): Option[String] = symtab.info(symbol)
    .map(Print.info(Format.Compact, _, printerSymtab))

  private def isDeclared(symbol: String): Boolean = symtab.info(symbol.owner).map(_.signature) match {
    case Some(sig: s.ClassSignature) => sig.declarations.exists(_.symlinks.contains(symbol))
    case sig => fail(s"expected ClassSignature for ${symbol.owner}, obtained $sig")
  }

  private def check(symbol: String, info: Option[String], declared: Boolean)(implicit
      loc: munit.Location,
  ): (s.TextDocument, String) => Unit = { (_, obtained) =>
    assertEquals(obtained, symbol)
    assertEquals(load(symbol), info)
    assertEquals(isDeclared(symbol), declared)
  }

  targeted(
    """|package i1752a
       |object A { val x, y: AnyRef = ""; x <<==>> y }
       |""".stripMargin,
    check("java/lang/Object#`==`().", None, false),
  )

  targeted(
    """|package i1752b
       |object A { val x, y: AnyRef = ""; x <<!=>> y }
       |""".stripMargin,
    check("java/lang/Object#`!=`().", None, false),
  )

  targeted(
    """|package i1752c
       |object A { val x: AnyRef = ""; x.<<##>> }
       |""".stripMargin,
    check("java/lang/Object#`##`().", None, false),
  )

  targeted(
    """|package i1752d
       |object A { val x, y: AnyRef = ""; x <<eq>> y }
       |""".stripMargin,
    check("java/lang/Object#eq().", None, false),
  )

  targeted(
    """|package i1752e
       |object A { val x, y: AnyRef = ""; x <<ne>> y }
       |""".stripMargin,
    check("java/lang/Object#ne().", None, false),
  )

  targeted(
    """|package i1752f
       |object A { val x: AnyRef = ""; x.<<synchronized>>(1) }
       |""".stripMargin,
    check("java/lang/Object#synchronized().", None, false),
  )

  targeted(
    """|package i1752g
       |object A { <<synchronized>>(1) }
       |""".stripMargin,
    check("java/lang/Object#synchronized().", None, false),
  )

  targeted(
    """|package i1752h
       |object A { val x: AnyRef = ""; x.<<isInstanceOf>>[String] }
       |""".stripMargin,
    check(
      "scala/Any#isInstanceOf().",
      Some("scala/Any#isInstanceOf(). => final method isInstanceOf[A](): Boolean"),
      true,
    ),
  )

  targeted(
    """|package i1752i
       |object A { val x: AnyRef = ""; x.<<asInstanceOf>>[String] }
       |""".stripMargin,
    check(
      "scala/Any#asInstanceOf().",
      Some("scala/Any#asInstanceOf(). => final method asInstanceOf[A](): A"),
      true,
    ),
  )

  targeted(
    """|package i1752j
       |object A { "a" <<==>> "b" }
       |""".stripMargin,
    check("java/lang/Object#`==`().", None, false),
  )

  targeted(
    """|package i1752k
       |object A { "a" <<+>> 1 }
       |""".stripMargin,
    check("java/lang/String#`+`().", None, false),
  )

  targeted(
    """|package i1752l
       |object A { val x: AnyRef = ""; x.<<hashCode>>() }
       |""".stripMargin,
    check(
      "java/lang/Object#hashCode().",
      Some("java/lang/Object#hashCode(). => method hashCode(): Int"),
      true,
    ),
  )

  targeted(
    """|package i1752m
       |object A { val x: Any = 1; x.<<##>> }
       |""".stripMargin,
    check("scala/Any#`##`().", Some("scala/Any#`##`(). => final method ##(): Int"), true),
  )

  private lazy val otherLibrary = {
    val otherVersion =
      if (ScalaVersion.is212) BuildInfo.latestScala213Version else BuildInfo.latestScala212Version
    Classpath(Jars.fetch("org.scala-lang", "scala-library", otherVersion))
  }

  test("scala-library of the other Scala version") {
    val otherSymtab = GlobalSymbolTable(otherLibrary, includeJdk = true)
    assertEquals(
      load(otherSymtab, "scala/Any#`##`()."),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
    assertEquals(load(otherSymtab, "java/lang/Object#`##`()."), None)
  }

  test("no scala-library") {
    val jdkSymtab = GlobalSymbolTable(Classpath(Nil), includeJdk = true)
    assertEquals(
      load(jdkSymtab, "scala/Any#`##`()."),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
    assertEquals(load(jdkSymtab, "java/lang/Object#`##`()."), None)
  }

  test("scala-library-synthetics of the other Scala version") {
    val out = AbsolutePath(Files.createTempDirectory("issue1752_"))
    val settings = Settings().withOut(out).withDependencyClasspath(otherLibrary)
      .withScalaLibrarySynthetics(true)
    val synthetics = Metacp.process(settings, Reporter().withSilentOut()).scalaLibrarySynthetics
    val info = synthetics
      .flatMap(PlatformFileIO.withJarFileSystem(_, create = false, close = true) { root =>
        val path = root.resolve("META-INF/semanticdb/scala/Any.class.semanticdb")
        s.TextDocuments.parseFrom(path.readAllBytes).documents.flatMap(_.symbols)
          .find(_.symbol == "scala/Any#`##`().")
      })
    assertEquals(
      info.map(Print.info(Format.Compact, _, printerSymtab)),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
  }

  test("scala-library-synthetics of the other Scala version with --usejavacp") {
    val out = AbsolutePath(Files.createTempDirectory("issue1752_"))
    val settings = Settings().withOut(out).withDependencyClasspath(otherLibrary)
      .withScalaLibrarySynthetics(true).withUsejavacp(true)
    val synthetics = Metacp.process(settings, Reporter().withSilentOut()).scalaLibrarySynthetics
    val info = synthetics
      .flatMap(PlatformFileIO.withJarFileSystem(_, create = false, close = true) { root =>
        val path = root.resolve("META-INF/semanticdb/scala/Any.class.semanticdb")
        s.TextDocuments.parseFrom(path.readAllBytes).documents.flatMap(_.symbols)
          .find(_.symbol == "scala/Any#`##`().")
      })
    // on 2.13, the 2.12 library comes first on the classpath, so `##()` is right
    assertEquals(
      info.map(Print.info(Format.Compact, _, printerSymtab)),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
  }

  test("metacp of a JDK jar") {
    val dir = Files.createTempDirectory("issue1752_")
    val jdkJar = dir.resolve("jdk.jar")
    val jrt = java.nio.file.FileSystems.getFileSystem(java.net.URI.create("jrt:/"))
    val jos = new java.util.jar.JarOutputStream(Files.newOutputStream(jdkJar))
    try List("Object", "String").foreach { name =>
        jos.putNextEntry(new java.util.jar.JarEntry(s"java/lang/$name.class"))
        jos.write(Files.readAllBytes(jrt.getPath(s"/modules/java.base/java/lang/$name.class")))
        jos.closeEntry()
      }
    finally jos.close()
    val out = AbsolutePath(dir.resolve("out"))
    val settings = Settings().withOut(out).withClasspath(Classpath(AbsolutePath(jdkJar)))
      .withDependencyClasspath(otherLibrary).withIncludeJdk(true)
    val result = Metacp.process(settings, Reporter().withSilentOut())
    val infos = result.classpath.toList.flatMap(_.entries).flatMap(jar =>
      PlatformFileIO.withJarFileSystem(jar, create = false, close = true)(root =>
        List("Object", "String").flatMap { name =>
          val path = root.resolve(s"META-INF/semanticdb/java/lang/$name.class.semanticdb")
          s.TextDocuments.parseFrom(path.readAllBytes).documents.flatMap(_.symbols)
        },
      ),
    )
    def find(sym: String) = infos.find(_.symbol == sym)
      .map(Print.info(Format.Compact, _, printerSymtab))
    assertEquals(find("java/lang/Object#`##`()."), None)
    assertEquals(find("java/lang/String#`+`()."), None)
  }

  test("classes directory of the other Scala version before scala-library") {
    val dir = Files.createTempDirectory("issue1752_")
    val marker = if (ScalaVersion.is212) "IterableOnce" else "TraversableOnce"
    Files.createDirectories(dir.resolve("scala/collection"))
    Files.createFile(dir.resolve("scala/Predef.class"))
    Files.createFile(dir.resolve(s"scala/collection/$marker.class"))
    val entries = AbsolutePath(dir) :: Library.scalaLibrary.classpath().entries
    val dirSymtab = GlobalSymbolTable(Classpath(entries), includeJdk = true)
    assertEquals(
      load(dirSymtab, "scala/Any#`##`()."),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
    assertEquals(load(dirSymtab, "java/lang/Object#`##`()."), None)
  }

  test("scala-library before the other Scala version") {
    val entries = Library.scalaLibrary.classpath().entries ++ otherLibrary.entries
    val bothSymtab = GlobalSymbolTable(Classpath(entries), includeJdk = true)
    assertEquals(
      load(bothSymtab, "scala/Any#`##`()."),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
    assertEquals(load(bothSymtab, "java/lang/Object#`##`()."), None)
  }

  test("pathing jar to the other Scala version") {
    val dir = Files.createTempDirectory("issue1752_")
    val pathing = dir.resolve("pathing.jar")
    val manifest = new java.util.jar.Manifest()
    val attributes = manifest.getMainAttributes
    attributes.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")
    attributes.put(java.util.jar.Attributes.Name.CLASS_PATH, otherLibrary.entries.head.toString)
    new java.util.jar.JarOutputStream(Files.newOutputStream(pathing), manifest).close()
    val entries = AbsolutePath(pathing) :: Library.scalaLibrary.classpath().entries
    val pathingSymtab = GlobalSymbolTable(Classpath(entries), includeJdk = true)
    assertEquals(
      load(pathingSymtab, "scala/Any#`##`()."),
      Some("scala/Any#`##`(). => final method ##(): Int"),
    )
    assertEquals(load(pathingSymtab, "java/lang/Object#`##`()."), None)
  }

}
