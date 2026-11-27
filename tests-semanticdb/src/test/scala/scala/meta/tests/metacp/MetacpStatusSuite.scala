package scala.meta.tests.metacp

import scala.meta.cli._
import scala.meta.internal.io.{InputStreamIO, PathIO}
import scala.meta.io._
import scala.meta.metacp.{Result, Settings}
import scala.meta.tests._
import scala.meta.tests.cli.CliTestUtils

import java.io.File.pathSeparator
import java.io.{ByteArrayOutputStream, IOException, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import java.util.Locale
import java.util.jar._
import java.util.zip.{ZipException, ZipFile}

import scala.util.Properties

import munit.FunSuite

class MetacpStatusSuite extends FunSuite {

  // inside the checkout, so that a relative path to it exists on Windows too
  private val tmp = Files
    .createTempDirectory(Files.createDirectories(Paths.get(BuildInfo.targetDirectory)), "metacp_")
  private val note = "NOTE. To fix 'missing symbol' errors please provide a complete " +
    "--classpath or --dependency-classpath. The provided classpath or classpaths should " +
    "include the Scala library as well as JDK jars such as rt.jar."

  private def relative(path: Path): String = PathIO.workingDirectory.toNIO.relativize(path).toString

  private def dir(name: String, files: (String, Array[Byte])*): Path = {
    val dir = Files.createDirectories(tmp.resolve(name))
    files.foreach { case (file, bytes) => Files.write(dir.resolve(file), bytes) }
    dir
  }

  private def jar(name: String, files: (String, Array[Byte])*): Path = {
    val jar = tmp.resolve(name)
    val os = new JarOutputStream(Files.newOutputStream(jar))
    try files.foreach { case (file, bytes) =>
        os.putNextEntry(new JarEntry(file))
        os.write(bytes)
      }
    finally os.close()
    jar
  }

  private def parse(out: Path, classpath: String, flags: String*): Settings = Settings
    .parse(List("--out", out.toString) ++ flags :+ classpath, Reporter()).get

  private def metacp(settings: Settings): (String, String) = {
    val (_, stdout, stderr) = CliTestUtils
      .withReporter(reporter => Metacp.process(settings, reporter))
    (stdout, stderr)
  }

  private def stdout(settings: Settings, charset: String): String = {
    val bytes = new ByteArrayOutputStream
    Metacp.process(settings, Reporter().withOut(new PrintStream(bytes, true, charset)).withSilentErr())
    new String(bytes.toByteArray, charset)
  }

  // an output path uses the separator of the platform
  private def status(entries: (String, String)*): String =
    rawStatus("", entries.map { case (in, out) => in -> s"${Paths.get(out)}" }: _*)
      .replace("\\", "\\\\")

  private def rawStatus(synthetics: String, entries: (String, String)*): String = entries
    .map { case (in, out) => s"""    "$in": "$out"""" }.mkString(
      "{\n  \"status\": {\n",
      ",\n",
      s"""\n  },\n  "scalaLibrarySynthetics": "$synthetics"\n}\n""",
    )

  private def unix(): Unit = assume(!Properties.isWin, "Windows rejects this file name")

  private def run(settings: Settings): (Result, String, String) = CliTestUtils
    .withReporter(reporter => Metacp.process(settings, reporter))

  private def classfile(name: String): (String, Array[Byte]) = s"com/javacp/$name.class" ->
    Files.readAllBytes(Paths.get(BuildInfo.databaseClasspath).resolve(s"com/javacp/$name.class"))

  private val aclass = Files.readAllBytes(Paths.get(BuildInfo.databaseClasspath).resolve("A.class"))

  private def manifest(classPath: String): (String, Array[Byte]) = {
    val manifest = new Manifest
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, classPath)
    val bytes = new ByteArrayOutputStream
    manifest.write(bytes)
    JarFile.MANIFEST_NAME -> bytes.toByteArray
  }

  private def manifestJars(name: String): Path = {
    val dir = this.dir(name)
    List("manifest.jar", "part0.jar", "part1.jar")
      .foreach(file => Files.copy(Utils.getResourceOpt(file).get, dir.resolve(file)))
    dir
  }

  /** The `Class-Path` of new empty jars in `name`, longer than the 512 bytes of a manifest line. */
  private def longClasspath(name: String): String = {
    dir(name)
    val links = Iterator.from(0).map(i => s"link-$i.jar")
      .scanLeft("")((classPath, jar) => if (classPath.isEmpty) jar else s"$classPath $jar")
      .find(_.length > 512).get
    links.split(' ').foreach(link => jar(s"$name/$link"))
    links
  }

  private def entries(jar: Path): List[String] = {
    val zip = new ZipFile(jar.toFile)
    val names = List.newBuilder[String]
    try {
      val it = zip.entries()
      while (it.hasMoreElements) names += it.nextElement().getName
    } finally zip.close()
    names.result().sorted
  }

  /** The `Class-Path` of the manifest that `JarInputStream` finds, or the message of its error. */
  private def classPath(jar: Path): Either[String, Option[String]] = {
    val in = Files.newInputStream(jar)
    try {
      val manifest = Option(new JarInputStream(in).getManifest)
      Right(manifest.flatMap(manifest => Option(manifest.getMainAttributes.getValue("Class-Path"))))
    } catch {
      // JDK 17 and later append the line number
      case e: IOException => Left(e.getMessage.replaceFirst(" \\(line \\d+\\)$", ""))
    } finally in.close()
  }

  private def errors(stderr: String): List[String] = stderr.linesIterator
    .filter(_.startsWith("error: ")).toList

  test("#1791: relative directory") {
    val in = relative(dir("reldir"))
    val out = tmp.resolve("out-reldir")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/reldir"))
    assertNoDiff(stderr, "")
  }

  test("#1791: relative jar") {
    val in = relative(jar("reljar.jar"))
    val out = tmp.resolve("out-reljar")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/reljar.jar"))
    assertNoDiff(stderr, "")
  }

  test("#1791: relative jar with a classfile") {
    val coin = Files
      .readAllBytes(Paths.get(BuildInfo.databaseClasspath).resolve("com/javacp/Coin.class"))
    val in = relative(jar("classjar.jar", "com/javacp/Coin.class" -> coin))
    val out = tmp.resolve("out-classjar")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/classjar.jar"))
    assertNoDiff(stderr, "")
  }

  test("#1791: absolute directory") {
    val in = dir("absdir").toString
    val out = tmp.resolve("out-absdir")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/absdir"))
    assertNoDiff(stderr, "")
  }

  test("#1791: relative classpath of two entries") {
    val a = relative(dir("cp-a"))
    val b = relative(jar("cp-b.jar"))
    val out = tmp.resolve("out-cp")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$b"))
    assertNoDiff(stdout, status(a -> s"$out/cp-a", b -> s"$out/cp-b.jar"))
    assertNoDiff(stderr, "")
  }

  test("#1791: relative entry passed twice") {
    val in = relative(dir("twice"))
    val out = tmp.resolve("out-twice")
    val (result, stdout, stderr) = run(parse(out, s"$in$pathSeparator$in"))
    assertNoDiff(stdout, status(in -> s"$out/twice", in -> s"$out/twice-1"))
    assertNoDiff(stderr, "")
    assertEquals(result.classpath, Some(Classpath(out.resolve("twice-1"))))
    assertEquals(Files.exists(out.resolve("twice-1")), true)
  }

  test("relative jar passed twice") {
    val in = relative(jar("twice.jar"))
    val out = tmp.resolve("out-twice-jar")
    val (result, stdout, stderr) = run(parse(out, s"$in$pathSeparator$in"))
    assertNoDiff(stdout, status(in -> s"$out/twice.jar", in -> s"$out/twice-1.jar"))
    assertNoDiff(stderr, "")
    assertEquals(result.classpath, Some(Classpath(out.resolve("twice-1.jar"))))
    assertEquals(Files.exists(out.resolve("twice-1.jar")), true)
  }

  test("#1791: relative and absolute spelling of one entry") {
    val abs = dir("spelling")
    val in = relative(abs)
    val out = tmp.resolve("out-spelling")
    val (result, stdout, stderr) = run(parse(out, s"$in$pathSeparator$abs"))
    assertNoDiff(stdout, status(in -> s"$out/spelling", s"$abs" -> s"$out/spelling-1"))
    assertNoDiff(stderr, "")
    assertEquals(result.classpath, Some(Classpath(out.resolve("spelling-1"))))
  }

  test("#1791: relative directory with a missing symbol") {
    val aclass = Files.readAllBytes(Paths.get(BuildInfo.databaseClasspath).resolve("A.class"))
    val in = relative(dir("missing", "A.class" -> aclass))
    val out = tmp.resolve("out-missing")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> ""))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $in
          |$note
          |""".stripMargin,
    )
  }

  test("#1791: relative directory with a broken classfile") {
    val in = relative(dir("broken", "Bad.class" -> Array[Byte](1, 2, 3)))
    val out = tmp.resolve("out-broken")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> ""))
    val bad = AbsolutePath(in).resolve("Bad.class")
    assertNoDiff(stderr.linesIterator.next(), s"error: can't convert $bad in $in")
  }

  test("#1791: relative jar with a manifest classpath") {
    val manifestDir = dir("manifest")
    List("manifest.jar", "part0.jar", "part1.jar")
      .foreach(name => Files.copy(Utils.getResourceOpt(name).get, manifestDir.resolve(name)))
    val in = relative(manifestDir.resolve("manifest.jar"))
    val out = tmp.resolve("out-manifest")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> ""))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $in
          |$note
          |""".stripMargin,
    )
    val jar = out.resolve("manifest.jar")
    assertEquals(entries(jar), List("META-INF/MANIFEST.MF"))
    assertEquals(classPath(jar), Right(Some("part0.jar part1.jar")))
    val zip = new ZipFile(jar.toFile)
    val bytes =
      try InputStreamIO.readBytes(zip.getInputStream(zip.getEntry(JarFile.MANIFEST_NAME)))
      finally zip.close()
    assertEquals(
      new String(bytes, UTF_8),
      "Manifest-Version: 1.0\r\nClass-Path: part0.jar part1.jar\r\n\r\n",
    )
  }

  test("#1791: relative classpath replaced with withClasspath") {
    val parsed = relative(dir("parsed"))
    val in = relative(dir("replaced"))
    val out = tmp.resolve("out-replaced")
    val (stdout, stderr) = metacp(parse(out, parsed).withClasspath(Classpath(in)))
    assertNoDiff(stdout, status(s"${AbsolutePath(in)}" -> s"$out/replaced"))
    assertNoDiff(stderr, "")
  }

  test("directory with a single quote and a space in its name") {
    val in = dir("it's here").toString
    val out = tmp.resolve("out-plain")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/it's here"))
    assertNoDiff(stderr, "")
  }

  test("directory with a backslash in its name") {
    assume(!Properties.isWin, "Windows reads a backslash as a separator")
    val in = dir("back\\slash").toString
    val out = tmp.resolve("out-backslash")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"$tmp/back\\\\slash" -> s"$out/back\\\\slash"))
    assertNoDiff(stderr, "")
  }

  test("directory with a double quote in its name") {
    unix()
    val in = dir("double\"quote").toString
    val out = tmp.resolve("out-quote")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"""$tmp/double\\"quote""" -> s"""$out/double\\"quote"""))
    assertNoDiff(stderr, "")
  }

  test("directory with a tab in its name") {
    unix()
    val in = dir("tab\tchar").toString
    val out = tmp.resolve("out-tab")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"$tmp/tab\\tchar" -> s"$out/tab\\tchar"))
    assertNoDiff(stderr, "")
  }

  test("directory with a unit separator in its name") {
    unix()
    val in = dir("unit\u001fsep").toString
    val out = tmp.resolve("out-unitsep")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"$tmp/unit\\u001fsep" -> s"$out/unit\\u001fsep"))
    assertNoDiff(stderr, "")
  }

  test("directory with non-ASCII characters in its name") {
    assume(Properties.isWin || System.getProperty("sun.jnu.encoding") == "UTF-8")
    val in = dir("café € 😀 \u007f").toString
    val out = tmp.resolve("out-nonascii")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(
      stdout,
      status(in -> s"$out/café € 😀 \u007f").replace("é € 😀", "\\u00e9 \\u20ac \\ud83d\\ude00"),
    )
    assertNoDiff(stderr, "")
  }

  private def nonAsciiStdout(charset: String, printed: String): Unit =
    test(s"directory with non-ASCII characters in its name, stdout in $charset") {
      assume(Properties.isWin || System.getProperty("sun.jnu.encoding") == "UTF-8")
      val name = "café € 😀 \u007f"
      val in = dir(name).toString
      val out = tmp.resolve(s"out-$charset")
      val expected = status(in -> s"$out/$name").replace(name, printed)
      assertNoDiff(stdout(parse(out, in), charset), expected)
    }

  nonAsciiStdout("US-ASCII", "caf\\u00e9 \\u20ac \\ud83d\\ude00 \u007f")
  nonAsciiStdout("windows-1252", "caf\\u00e9 \\u20ac \\ud83d\\ude00 \u007f")

  test("directory with a lone surrogate in its name") {
    assume(Properties.isWin, "only Windows accepts this file name")
    val in = dir("lone\ud800").toString
    val out = tmp.resolve("out-lone")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/lone\ud800").replace("\ud800", "\\ud800"))
    assertNoDiff(stderr, "")
  }

  test("scala library synthetics in an output directory with a double quote") {
    unix()
    val in = dir("synthetics").toString
    val out = tmp.resolve("out\"synthetics")
    val (stdout, stderr) = metacp(parse(out, in, "--include-scala-library-synthetics"))
    val synthetics = s"""$tmp/out\\"synthetics/scala-library-synthetics.jar"""
    assertNoDiff(stdout, rawStatus(synthetics, in -> s"""$tmp/out\\"synthetics/synthetics"""))
    assertNoDiff(stderr, "")
  }

  test("#1791: two ASCII directories, one with a DEL in its name, stdout bytes") {
    val a = dir("bytes-a").toString
    val b = dir("bytes\u007fb").toString
    val out = tmp.resolve("out-bytes")
    val expected = status(a -> s"$out/bytes-a", b -> s"$out/bytes\u007fb")
    assertEquals(
      stdout(parse(out, s"$a$pathSeparator$b"), "US-ASCII"),
      expected.replace("\n", System.lineSeparator()),
    )
  }

  test("#1791: directory with a backspace, form feed, newline and return in its name") {
    unix()
    val in = dir("ctl\b\f\n\rend").toString
    val out = tmp.resolve("out-ctl")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"$tmp/ctl\\b\\f\\n\\rend" -> s"$out/ctl\\b\\f\\n\\rend"))
    assertNoDiff(stderr, "")
  }

  test("#1791: directory with U+0080, U+0085, U+2028 and U+FFFD in its name") {
    assume(Properties.isWin || System.getProperty("sun.jnu.encoding") == "UTF-8")
    val name = "edge\u0080\u0085 �"
    val in = dir(name).toString
    val out = tmp.resolve("out-edge")
    val (stdout, stderr) = metacp(parse(out, in))
    val escaped = "edge\\u0080\\u0085\\u2028\\ufffd"
    assertNoDiff(stdout, status(in -> s"$out/$name").replace(name, escaped))
    assertNoDiff(stderr, "")
  }

  test("jar with a classfile and a manifest classpath") {
    dir("own")
    jar("own/linked.jar", classfile("Logger"))
    val in = jar("own/own.jar", manifest("linked.jar"), classfile("Coin")).toString
    val out = tmp.resolve("out-own")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/own.jar"))
    assertNoDiff(stderr, "")
    assertEquals(entries(out.resolve("own.jar")), List("META-INF/MANIFEST.MF"))
    assertEquals(classPath(out.resolve("own.jar")), Right(Some("linked.jar")))
    assertEquals(
      entries(out.resolve("linked.jar")),
      List(
        "META-INF/",
        "META-INF/semanticdb/",
        "META-INF/semanticdb/com/",
        "META-INF/semanticdb/com/javacp/",
        "META-INF/semanticdb/com/javacp/Logger.class.semanticdb",
      ),
    )
  }

  test("directory spelled with `..`") {
    val in = dir("dotdot").toString
    val again = Paths.get(in, "..", "dotdot").toString
    val out = tmp.resolve("out-dotdot")
    val (result, stdout, stderr) = run(parse(out, s"$in$pathSeparator$again"))
    assertNoDiff(stdout, status(in -> s"$out/dotdot", again -> s"$out/dotdot-1"))
    assertNoDiff(stderr, "")
    val outputs = List(out.resolve("dotdot"), out.resolve("dotdot-1")).map(AbsolutePath(_))
    assertEquals(result.classpath, Some(Classpath(outputs)))
  }

  test("jar spelled in two cases") {
    dir("Spelled")
    val in = jar("Spelled/spelled.jar").toString
    val other = tmp.resolve("spelled").resolve("spelled.jar")
    assume(Files.exists(other), "the file system is case-sensitive")
    val out = tmp.resolve("out-spelled")
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$other"))
    assertNoDiff(stdout, status(in -> s"$out/spelled.jar", s"$other" -> s"$out/spelled-1.jar"))
    assertNoDiff(stderr, "")
  }

  test("symlink to a jar on the classpath") {
    assume(!Properties.isWin, "Windows needs a privilege to create a symlink")
    dir("symlink")
    val target = jar("symlink/sym.jar", classfile("Coin"))
    val link = Files.createSymbolicLink(dir("symlink-link").resolve("sym.jar"), target)
    val out = tmp.resolve("out-symlink")
    val (stdout, stderr) = metacp(parse(out, s"$target$pathSeparator$link"))
    assertNoDiff(stdout, status(s"$target" -> s"$out/sym.jar", s"$link" -> s"$out/sym-1.jar"))
    assertNoDiff(stderr, "")
  }

  test("jar on the classpath and linked from an earlier manifest") {
    dir("earlier")
    val linked = jar("earlier/linked.jar", classfile("Logger")).toString
    val own = jar("earlier/own.jar", manifest("linked.jar"), classfile("Coin")).toString
    val out = tmp.resolve("out-earlier")
    val (stdout, stderr) = metacp(parse(out, s"$own$pathSeparator$linked"))
    assertNoDiff(stdout, status(own -> s"$out/own.jar", linked -> s"$out/linked-1.jar"))
    assertNoDiff(stderr, "")
    assertEquals(entries(out.resolve("own.jar")), List("META-INF/MANIFEST.MF"))
    assertEquals(classPath(out.resolve("own.jar")), Right(Some("linked.jar")))
  }

  test("jar on the classpath and linked from a later manifest") {
    dir("later")
    val linked = jar("later/linked.jar", classfile("Logger")).toString
    val own = jar("later/own.jar", manifest("linked.jar"), classfile("Coin")).toString
    val out = tmp.resolve("out-later")
    val (stdout, stderr) = metacp(parse(out, s"$linked$pathSeparator$own"))
    assertNoDiff(stdout, status(linked -> s"$out/linked.jar", own -> s"$out/own.jar"))
    assertNoDiff(stderr, "")
    assertEquals(entries(out.resolve("own.jar")), List("META-INF/MANIFEST.MF"))
    assertEquals(classPath(out.resolve("own.jar")), Right(Some("linked-1.jar")))
  }

  test("two manifests linking one jar") {
    dir("diamond")
    jar("diamond/c.jar", classfile("Logger"))
    val a = jar("diamond/a.jar", manifest("c.jar")).toString
    val b = jar("diamond/b.jar", manifest("c.jar")).toString
    val out = tmp.resolve("out-diamond")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$b"))
    assertNoDiff(stdout, status(a -> s"$out/a.jar", b -> s"$out/b.jar"))
    assertNoDiff(stderr, "")
    assertEquals(classPath(out.resolve("a.jar")), Right(Some("c.jar")))
    assertEquals(classPath(out.resolve("b.jar")), Right(Some("c-1.jar")))
    assertEquals(Files.exists(out.resolve("c-1.jar")), true)
  }

  test("manifest chain of three jars") {
    dir("chain")
    jar("chain/c.jar", classfile("Logger"))
    jar("chain/b.jar", manifest("c.jar"), classfile("Coin"))
    val in = jar("chain/a.jar", manifest("b.jar")).toString
    val out = tmp.resolve("out-chain")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/a.jar"))
    assertNoDiff(stderr, "")
    assertEquals(Files.exists(out.resolve("c.jar")), false)
    assertEquals(classPath(out.resolve("a.jar")), Right(Some("b.jar")))
    assertEquals(
      entries(out.resolve("b.jar")),
      List(
        "META-INF/",
        "META-INF/semanticdb/",
        "META-INF/semanticdb/com/",
        "META-INF/semanticdb/com/javacp/",
        "META-INF/semanticdb/com/javacp/Coin.class.semanticdb",
        "META-INF/semanticdb/com/javacp/Logger.class.semanticdb",
      ),
    )
    assertEquals(classPath(out.resolve("b.jar")), Right(None))
  }

  test("missing symbol in a jar on the classpath and linked") {
    val jars = manifestJars("linked-missing")
    val part0 = jars.resolve("part0.jar").toString
    val manifest = jars.resolve("manifest.jar").toString
    val out = tmp.resolve("out-linked-missing")
    val (stdout, stderr) = metacp(parse(out, s"$part0$pathSeparator$manifest"))
    assertNoDiff(stdout, status(part0 -> "", manifest -> s"$out/manifest.jar"))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $part0
          |$note
          |""".stripMargin,
    )
  }

  test("jar linked from a manifest and on the classpath") {
    val jars = manifestJars("manifest-then-part0")
    val manifest = jars.resolve("manifest.jar").toString
    val part0 = jars.resolve("part0.jar").toString
    val out = tmp.resolve("out-manifest-then-part0")
    val (stdout, stderr) = metacp(parse(out, s"$manifest$pathSeparator$part0"))
    assertNoDiff(stdout, status(manifest -> "", part0 -> s"$out/part0-1.jar"))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $manifest
          |$note
          |""".stripMargin,
    )
  }

  test("broken classfile in a linked jar") {
    dir("linked-broken")
    jar("linked-broken/bad.jar", "Bad.class" -> Array[Byte](1, 2, 3))
    val in = jar("linked-broken/own.jar", manifest("bad.jar")).toString
    val out = tmp.resolve("out-linked-broken")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> ""))
    assertEquals(
      errors(stderr),
      List(
        s"error: can't convert /Bad.class in $in",
        s"error: can't convert /Bad.class in ${tmp.resolve("linked-broken/bad.jar")}",
      ),
    )
  }

  test("classpath entry that does not exist") {
    val dir = tmp.resolve("absent").toString
    val jar = tmp.resolve("absent.jar").toString
    val out = tmp.resolve("out-absent")
    val (stdout, stderr) = metacp(parse(out, s"$dir$pathSeparator$jar"))
    assertNoDiff(stdout, status(dir -> s"$out/absent", jar -> s"$out/absent.jar"))
    assertNoDiff(stderr, "")
  }

  test("manifest that links a missing jar and a directory") {
    dir("links/linked")
    val in = jar("links/own.jar", manifest("missing.jar linked")).toString
    val out = tmp.resolve("out-links")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/own.jar"))
    assertNoDiff(stderr, "")
    assertEquals(entries(out.resolve("own.jar")), List("META-INF/MANIFEST.MF"))
    assertEquals(classPath(out.resolve("own.jar")), Right(Some("linked")))
  }

  test("missing symbol in two directories") {
    val a = dir("missing2-a", "A.class" -> aclass).toString
    val b = dir("missing2-b", "A.class" -> aclass).toString
    val out = tmp.resolve("out-missing2")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$b"))
    assertNoDiff(stdout, status(a -> "", b -> s"$out/missing2-b"))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $a
          |$note
          |""".stripMargin,
    )
  }

  test("directory with a missing symbol passed twice") {
    val in = dir("missing-twice", "A.class" -> aclass).toString
    val out = tmp.resolve("out-missing-twice")
    val (result, stdout, stderr) = run(parse(out, s"$in$pathSeparator$in"))
    assertNoDiff(stdout, status(in -> "", in -> s"$out/missing-twice-1"))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $in
          |$note
          |""".stripMargin,
    )
    assertEquals(result.isSuccess, true)
    assertEquals(result.classpath, Some(Classpath(out.resolve("missing-twice-1"))))
  }

  test("jar on the classpath and linked through `..`") {
    dir("updir/lib")
    dir("updir/app")
    val x = jar("updir/lib/x.jar", classfile("Logger")).toString
    val a = jar("updir/app/a.jar", manifest("../lib/x.jar"), classfile("Coin")).toString
    val out = tmp.resolve("out-updir")
    val (stdout, stderr) = metacp(parse(out, s"$x$pathSeparator$a"))
    assertNoDiff(stdout, status(x -> s"$out/x.jar", a -> s"$out/a.jar"))
    assertNoDiff(stderr, "")
    assertEquals(entries(out.resolve("a.jar")), List("META-INF/MANIFEST.MF"))
    assertEquals(classPath(out.resolve("a.jar")), Right(Some("x-1.jar")))
  }

  test("two manifests linking one jar with a missing symbol") {
    dir("diamond-missing")
    jar("diamond-missing/c.jar", "A.class" -> aclass)
    val a = jar("diamond-missing/a.jar", manifest("c.jar")).toString
    val b = jar("diamond-missing/b.jar", manifest("c.jar")).toString
    val out = tmp.resolve("out-diamond-missing")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$b"))
    assertNoDiff(stdout, status(a -> "", b -> s"$out/b.jar"))
    assertNoDiff(
      stderr,
      s"""|missing symbol: scala in $a
          |$note
          |""".stripMargin,
    )
  }

  test("link with the name of a later entry") {
    dir("near/q")
    dir("near/r")
    jar("near/q/x.jar", classfile("Logger"))
    val own = jar("near/own.jar", manifest("q/x.jar")).toString
    val x = jar("near/r/x.jar").toString
    val out = tmp.resolve("out-near")
    val (stdout, stderr) = metacp(parse(out, s"$own$pathSeparator$x"))
    assertNoDiff(stdout, status(own -> s"$out/own.jar", x -> s"$out/x-1.jar"))
    assertNoDiff(stderr, "")
    assertEquals(classPath(out.resolve("own.jar")), Right(Some("x.jar")))
    assertEquals(
      entries(out.resolve("x.jar")),
      List(
        "META-INF/",
        "META-INF/semanticdb/",
        "META-INF/semanticdb/com/",
        "META-INF/semanticdb/com/javacp/",
        "META-INF/semanticdb/com/javacp/Logger.class.semanticdb",
      ),
    )
    assertEquals(entries(out.resolve("x-1.jar")), List("META-INF/", "META-INF/semanticdb/"))
  }

  test("link of a link with the name of a later entry") {
    dir("deep/x")
    dir("deep/y")
    jar("deep/y/c.jar", classfile("Logger"))
    jar("deep/b.jar", manifest("y/c.jar"), classfile("Coin"))
    val a = jar("deep/a.jar", manifest("b.jar")).toString
    val c = jar("deep/x/c.jar").toString
    val out = tmp.resolve("out-deep")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$c"))
    assertNoDiff(stdout, status(a -> s"$out/a.jar", c -> s"$out/c.jar"))
    assertNoDiff(stderr, "")
    assertEquals(entries(out.resolve("c.jar")), List("META-INF/", "META-INF/semanticdb/"))
    assertEquals(Files.exists(out.resolve("c-1.jar")), false)
    assertEquals(
      entries(out.resolve("b.jar")),
      List(
        "META-INF/",
        "META-INF/semanticdb/",
        "META-INF/semanticdb/com/",
        "META-INF/semanticdb/com/javacp/",
        "META-INF/semanticdb/com/javacp/Coin.class.semanticdb",
        "META-INF/semanticdb/com/javacp/Logger.class.semanticdb",
      ),
    )
    assertEquals(classPath(out.resolve("b.jar")), Right(None))
  }

  test("jar passed twice, then a jar of the same name") {
    dir("same/y")
    val x = jar("same/x.jar", classfile("Coin")).toString
    val y = jar("same/y/x.jar", classfile("Logger")).toString
    val out = tmp.resolve("out-same")
    val (stdout, stderr) = metacp(parse(out, List(x, x, y).mkString(pathSeparator)))
    assertNoDiff(stdout, status(x -> s"$out/x.jar", x -> s"$out/x-1.jar", y -> s"$out/x-2.jar"))
    assertNoDiff(stderr, "")
    assertEquals(
      entries(out.resolve("x-1.jar")),
      List(
        "META-INF/",
        "META-INF/semanticdb/",
        "META-INF/semanticdb/com/",
        "META-INF/semanticdb/com/javacp/",
        "META-INF/semanticdb/com/javacp/Coin.class.semanticdb",
      ),
    )
    assertEquals(Files.exists(out.resolve("x-2.jar")), true)
  }

  test("directory with a broken classfile passed twice") {
    val in = dir("broken-twice", "Bad.class" -> Array[Byte](1, 2, 3)).toString
    val out = tmp.resolve("out-broken-twice")
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$in"))
    assertNoDiff(stdout, status(in -> "", in -> ""))
    val bad = AbsolutePath(in).resolve("Bad.class")
    assertEquals(
      errors(stderr),
      List(s"error: can't convert $bad in $in", s"error: can't convert $bad in $in"),
    )
  }

  test("jar with a manifest classpath over 72 bytes") {
    val head = "a-linked-jar-whose-name-is-long-enough-to-wrap-the-manifest-"
    val tail = "line.jar"
    dir("wrap")
    jar(s"wrap/$head$tail")
    val in = jar("wrap/own.jar", manifest(s"$head$tail")).toString
    val out = tmp.resolve("out-wrap")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/own.jar"))
    assertNoDiff(stderr, "")
    val zip = new ZipFile(out.resolve("own.jar").toFile)
    val bytes =
      try InputStreamIO.readBytes(zip.getInputStream(zip.getEntry(JarFile.MANIFEST_NAME)))
      finally zip.close()
    val text = new String(bytes, UTF_8)
    assertEquals(text, s"Manifest-Version: 1.0\r\nClass-Path: $head\r\n $tail\r\n\r\n")
  }

  test("jar with a long manifest classpath") {
    val links = longClasspath("long")
    val in = jar("long/long.jar", manifest(links)).toString
    val out = tmp.resolve("out-long")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/long.jar"))
    assertNoDiff(stderr, "")
    assertEquals(classPath(out.resolve("long.jar")), Right(Some(links)))
  }

  test("linked jar with a long manifest classpath") {
    val links = longClasspath("long-linked")
    jar("long-linked/b.jar", manifest(links))
    val in = jar("long-linked/a.jar", manifest("b.jar")).toString
    val out = tmp.resolve("out-long-linked")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/a.jar"))
    assertNoDiff(stderr, "")
    assertEquals(classPath(out.resolve("a.jar")), Right(Some("b.jar")))
    assertEquals(classPath(out.resolve("b.jar")), Right(None))
  }

  test("directory with broken signatures passed twice") {
    val in = dir("signatures", "A.class" -> aclass).toString
    val out = tmp.resolve("out-signatures")
    val flags = List("--stub-broken-signatures", "--log-broken-signatures")
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$in", flags: _*))
    assertNoDiff(stdout, status(in -> s"$out/signatures", in -> s"$out/signatures-1"))
    assertNoDiff(
      stderr,
      """|broken signature for _empty_/A#: missing symbol: scala
         |broken signature for _empty_/A#b().: missing symbol: <empty>.B
         |broken signature for _empty_/A#: missing symbol: scala
         |broken signature for _empty_/A#b().: missing symbol: <empty>.B
         |""".stripMargin,
    )
  }

  test("linked jar with broken signatures") {
    dir("linked-signatures")
    jar("linked-signatures/a.jar", "A.class" -> aclass)
    val in = jar("linked-signatures/own.jar", manifest("a.jar")).toString
    val out = tmp.resolve("out-linked-signatures")
    val flags = List("--stub-broken-signatures", "--log-broken-signatures")
    val (stdout, stderr) = metacp(parse(out, in, flags: _*))
    assertNoDiff(stdout, status(in -> s"$out/own.jar"))
    assertNoDiff(
      stderr,
      """|broken signature for _empty_/A#: missing symbol: scala
         |broken signature for _empty_/A#b().: missing symbol: <empty>.B
         |broken signature for _empty_/A#: missing symbol: scala
         |broken signature for _empty_/A#b().: missing symbol: <empty>.B
         |""".stripMargin,
    )
  }

  test("jars whose names differ in case") {
    dir("case-x")
    dir("case-y")
    val upper = jar("case-x/Lib.jar")
    val ignoresCase = Files.exists(upper.resolveSibling("lib.jar"))
    val lower = jar("case-y/lib.jar")
    val out = tmp.resolve("out-case")
    val (stdout, stderr) = metacp(parse(out, s"$upper$pathSeparator$lower"))
    val second = if (ignoresCase) "lib-1.jar" else "lib.jar"
    assertNoDiff(stdout, status(s"$upper" -> s"$out/Lib.jar", s"$lower" -> s"$out/$second"))
    assertNoDiff(stderr, "")
  }

  test("jars whose names differ in the case of an I, in a Turkish locale") {
    dir("turkish-x")
    dir("turkish-y")
    val upper = jar("turkish-x/LIB.jar")
    val ignoresCase = Files.exists(upper.resolveSibling("lib.jar"))
    val lower = jar("turkish-y/lib.jar")
    val out = tmp.resolve("out-turkish")
    val locale = Locale.getDefault
    Locale.setDefault(Locale.forLanguageTag("tr"))
    val (stdout, stderr) =
      try metacp(parse(out, s"$upper$pathSeparator$lower"))
      finally Locale.setDefault(locale)
    val second = if (ignoresCase) "lib-1.jar" else "lib.jar"
    assertNoDiff(stdout, status(s"$upper" -> s"$out/LIB.jar", s"$lower" -> s"$out/$second"))
    assertNoDiff(stderr, "")
  }

  test("directories whose names differ in case") {
    val upper = dir("dircase-x/Foo")
    val ignoresCase = Files.exists(upper.resolveSibling("foo"))
    val lower = dir("dircase-y/foo")
    val out = tmp.resolve("out-dircase")
    val (stdout, stderr) = metacp(parse(out, s"$upper$pathSeparator$lower"))
    val second = if (ignoresCase) "foo-1" else "foo"
    assertNoDiff(stdout, status(s"$upper" -> s"$out/Foo", s"$lower" -> s"$out/$second"))
    assertNoDiff(stderr, "")
  }

  test("corrupt jar between two directories") {
    val missing = dir("corrupt-missing", "A.class" -> aclass).toString
    val corrupt = dir("corrupt").resolve("corrupt.jar")
    Files.write(corrupt, Array[Byte](1, 2, 3))
    val last = dir("corrupt-last").toString
    val out = tmp.resolve("out-corrupt")
    val classpath = List(missing, corrupt.toString, last).mkString(pathSeparator)
    val err = new ByteArrayOutputStream
    val reporter = Reporter().withSilentOut().withErr(new PrintStream(err, true, "UTF-8"))
    intercept[ZipException](Metacp.process(parse(out, classpath), reporter))
    assertNoDiff(err.toString("UTF-8"), s"missing symbol: scala in $missing")
    assertEquals(Files.exists(out.resolve("corrupt-last")), false)
  }

  test("output directory of an earlier run") {
    val in = dir("earlier-run").toString
    val out = tmp.resolve("out-earlier-run")
    metacp(parse(out, in))
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/earlier-run-1"))
    assertNoDiff(stderr, "")
  }

  test("jar with a manifest classpath in an output directory of an earlier run") {
    dir("rerun")
    jar("rerun/linked.jar", classfile("Logger"))
    val in = jar("rerun/own.jar", manifest("linked.jar"), classfile("Coin")).toString
    val out = tmp.resolve("out-rerun")
    metacp(parse(out, in))
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/own-1.jar"))
    assertNoDiff(stderr, "")
    assertEquals(classPath(out.resolve("own-1.jar")), Right(Some("linked-1.jar")))
    assertEquals(Files.exists(out.resolve("linked-1.jar")), true)
  }

  test("directories of one name and a directory with its suffixed name") {
    val x = dir("suffix-x/classes").toString
    val y = dir("suffix-y/classes").toString
    val z = dir("suffix-z/classes-1").toString
    val out = tmp.resolve("out-suffix")
    val (stdout, stderr) = metacp(parse(out, List(x, y, z).mkString(pathSeparator)))
    assertNoDiff(
      stdout,
      status(x -> s"$out/classes", y -> s"$out/classes-1", z -> s"$out/classes-1-1"),
    )
    assertNoDiff(stderr, "")
  }

  test("directory with a suffixed name, then two directories of its base name") {
    val z = dir("suffixed-z/classes-1").toString
    val x = dir("suffixed-x/classes").toString
    val y = dir("suffixed-y/classes").toString
    val out = tmp.resolve("out-suffixed")
    val (stdout, stderr) = metacp(parse(out, List(z, x, y).mkString(pathSeparator)))
    assertNoDiff(stdout, status(z -> s"$out/classes-1", x -> s"$out/classes", y -> s"$out/classes-2"))
    assertNoDiff(stderr, "")
  }

  test("output directory that holds the input jars") {
    dir("inside")
    val a = jar("inside/a.jar", classfile("Coin"))
    val b = jar("inside/b.jar", classfile("Logger"))
    val bytes = List(a, b).map(Files.readAllBytes(_).toList)
    val out = tmp.resolve("inside")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$b"))
    assertNoDiff(stdout, status(s"$a" -> s"$out/a-1.jar", s"$b" -> s"$out/b-1.jar"))
    assertNoDiff(stderr, "")
    assertEquals(List(a, b).map(Files.readAllBytes(_).toList), bytes)
  }

  test("scala library synthetics in an output directory of an earlier run") {
    val in = dir("synthetics-again").toString
    val out = tmp.resolve("out-synthetics-again")
    metacp(parse(out, in, "--include-scala-library-synthetics"))
    val (result, _, stderr) = run(parse(out, in, "--include-scala-library-synthetics"))
    val synthetics = AbsolutePath(out.resolve("scala-library-synthetics-1.jar"))
    assertEquals(result.scalaLibrarySynthetics, Some(synthetics))
    assertNoDiff(stderr, "")
  }

}
