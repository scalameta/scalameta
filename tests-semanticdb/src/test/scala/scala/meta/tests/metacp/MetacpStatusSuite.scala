package scala.meta.tests.metacp

import scala.meta.cli._
import scala.meta.internal.io.PathIO
import scala.meta.io._
import scala.meta.metacp.Settings
import scala.meta.tests._
import scala.meta.tests.cli.CliTestUtils

import java.io.File.pathSeparator
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file._
import java.util.jar.{JarEntry, JarOutputStream}

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

  private def rawStatus(synthetics: String, entries: (String, String)*): String = entries
    .map { case (in, out) => s"""    "$in": "$out"""" }.mkString(
      "{\n  \"status\": {\n",
      ",\n",
      s"""\n  },\n  "scalaLibrarySynthetics": "$synthetics"\n}\n""",
    )

  private def unix(): Unit = assume(!Properties.isWin, "Windows rejects this file name")

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
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$in"))
    assertNoDiff(stdout, status(in -> s"$out/twice", in -> s"$out/twice-1"))
    assertNoDiff(stderr, "")
  }

  test("#1791: relative and absolute spelling of one entry") {
    val abs = dir("spelling")
    val in = relative(abs)
    val out = tmp.resolve("out-spelling")
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$abs"))
    assertNoDiff(stdout, status(in -> s"$out/spelling", s"$abs" -> s"$out/spelling-1"))
    assertNoDiff(stderr, "")
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
    assertNoDiff(stdout, rawStatus("", s"$tmp/back\\slash" -> s"$out/back\\slash"))
    assertNoDiff(stderr, "")
  }

  test("directory with a double quote in its name") {
    unix()
    val in = dir("double\"quote").toString
    val out = tmp.resolve("out-quote")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"""$tmp/double"quote""" -> s"""$out/double"quote"""))
    assertNoDiff(stderr, "")
  }

  test("directory with a tab in its name") {
    unix()
    val in = dir("tab\tchar").toString
    val out = tmp.resolve("out-tab")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"$tmp/tab\tchar" -> s"$out/tab\tchar"))
    assertNoDiff(stderr, "")
  }

  test("directory with a unit separator in its name") {
    unix()
    val in = dir("unit\u001fsep").toString
    val out = tmp.resolve("out-unitsep")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, rawStatus("", s"$tmp/unit\u001fsep" -> s"$out/unit\u001fsep"))
    assertNoDiff(stderr, "")
  }

  test("directory with non-ASCII characters in its name") {
    assume(Properties.isWin || System.getProperty("sun.jnu.encoding") == "UTF-8")
    val in = dir("café € 😀 \u007f").toString
    val out = tmp.resolve("out-nonascii")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/café € 😀 \u007f"))
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

  nonAsciiStdout("US-ASCII", "caf? ? ? \u007f")
  nonAsciiStdout("windows-1252", "café € ? \u007f")

  test("directory with a lone surrogate in its name") {
    assume(Properties.isWin, "only Windows accepts this file name")
    val in = dir("lone\ud800").toString
    val out = tmp.resolve("out-lone")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/lone\ud800").replace("\ud800", "?"))
    assertNoDiff(stderr, "")
  }

  test("scala library synthetics in an output directory with a double quote") {
    unix()
    val in = dir("synthetics").toString
    val out = tmp.resolve("out\"synthetics")
    val (stdout, stderr) = metacp(parse(out, in, "--include-scala-library-synthetics"))
    val synthetics = s"""$tmp/out"synthetics/scala-library-synthetics.jar"""
    assertNoDiff(stdout, rawStatus(synthetics, in -> s"""$tmp/out"synthetics/synthetics"""))
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
    assertNoDiff(stdout, rawStatus("", s"$tmp/ctl\b\f\n\rend" -> s"$out/ctl\b\f\n\rend"))
    assertNoDiff(stderr, "")
  }

  test("#1791: directory with U+0080, U+0085, U+2028 and U+FFFD in its name") {
    assume(Properties.isWin || System.getProperty("sun.jnu.encoding") == "UTF-8")
    val name = "edge\u0080\u0085 �"
    val in = dir(name).toString
    val out = tmp.resolve("out-edge")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/$name"))
    assertNoDiff(stderr, "")
  }

}
