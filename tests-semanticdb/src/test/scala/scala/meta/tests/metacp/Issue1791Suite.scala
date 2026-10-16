package scala.meta.tests.metacp

import scala.meta.cli._
import scala.meta.internal.io.PathIO
import scala.meta.io._
import scala.meta.metacp.Settings
import scala.meta.tests._
import scala.meta.tests.cli.CliTestUtils

import java.io.File.pathSeparator
import java.nio.file._
import java.util.jar.{JarEntry, JarOutputStream}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1791
class Issue1791Suite extends FunSuite {

  private val tmp = Files.createTempDirectory("issue1791_")
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

  private def parse(out: Path, classpath: String): Settings = Settings
    .parse(List("--out", out.toString, classpath), Reporter()).get

  private def metacp(settings: Settings): (String, String) = {
    val (_, stdout, stderr) = CliTestUtils
      .withReporter(reporter => Metacp.process(settings, reporter))
    (stdout, stderr)
  }

  private def status(entries: (String, String)*): String = entries.map { case (in, out) =>
    s"""    "$in": "$out""""
  }.mkString("{\n  \"status\": {\n", ",\n", "\n  },\n  \"scalaLibrarySynthetics\": \"\"\n}\n")

  test("relative directory") {
    val in = relative(dir("reldir"))
    val out = tmp.resolve("out-reldir")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/reldir"))
    assertNoDiff(stderr, "")
  }

  test("relative jar") {
    val in = relative(jar("reljar.jar"))
    val out = tmp.resolve("out-reljar")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/reljar.jar"))
    assertNoDiff(stderr, "")
  }

  test("relative jar with a classfile") {
    val coin = Files
      .readAllBytes(Paths.get(BuildInfo.databaseClasspath).resolve("com/javacp/Coin.class"))
    val in = relative(jar("classjar.jar", "com/javacp/Coin.class" -> coin))
    val out = tmp.resolve("out-classjar")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/classjar.jar"))
    assertNoDiff(stderr, "")
  }

  test("absolute directory") {
    val in = dir("absdir").toString
    val out = tmp.resolve("out-absdir")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> s"$out/absdir"))
    assertNoDiff(stderr, "")
  }

  test("relative classpath of two entries") {
    val a = relative(dir("cp-a"))
    val b = relative(jar("cp-b.jar"))
    val out = tmp.resolve("out-cp")
    val (stdout, stderr) = metacp(parse(out, s"$a$pathSeparator$b"))
    assertNoDiff(stdout, status(a -> s"$out/cp-a", b -> s"$out/cp-b.jar"))
    assertNoDiff(stderr, "")
  }

  test("relative entry passed twice") {
    val in = relative(dir("twice"))
    val out = tmp.resolve("out-twice")
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$in"))
    assertNoDiff(stdout, status(in -> s"$out/twice-1", in -> s"$out/twice-1"))
    assertNoDiff(stderr, "")
  }

  test("relative and absolute spelling of one entry") {
    val abs = dir("spelling")
    val in = relative(abs)
    val out = tmp.resolve("out-spelling")
    val (stdout, stderr) = metacp(parse(out, s"$in$pathSeparator$abs"))
    assertNoDiff(stdout, status(in -> s"$out/spelling", s"$abs" -> s"$out/spelling-1"))
    assertNoDiff(stderr, "")
  }

  test("relative directory with a missing symbol") {
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

  test("relative directory with a broken classfile") {
    val in = relative(dir("broken", "Bad.class" -> Array[Byte](1, 2, 3)))
    val out = tmp.resolve("out-broken")
    val (stdout, stderr) = metacp(parse(out, in))
    assertNoDiff(stdout, status(in -> ""))
    val bad = AbsolutePath(in).resolve("Bad.class")
    assertNoDiff(stderr.linesIterator.next(), s"error: can't convert $bad in $in")
  }

  test("relative jar with a manifest classpath") {
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

  test("relative classpath replaced with withClasspath") {
    val parsed = relative(dir("parsed"))
    val in = relative(dir("replaced"))
    val out = tmp.resolve("out-replaced")
    val (stdout, stderr) = metacp(parse(out, parsed).withClasspath(Classpath(in)))
    assertNoDiff(stdout, status(s"${AbsolutePath(in)}" -> s"$out/replaced"))
    assertNoDiff(stderr, "")
  }

}
