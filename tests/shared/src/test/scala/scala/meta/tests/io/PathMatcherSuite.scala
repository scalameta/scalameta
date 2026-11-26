package scala.meta.tests.io

import scala.meta.internal.io.PlatformFlags.{isJS, isNative}
import scala.meta.internal.io.{PathIO, PathMatcher, PlatformFlags}

import java.nio.file.Paths

import scala.util.{Failure, Success, Try}

import munit.FunSuite

class PathMatcherSuite extends FunSuite {

  private def matches(pattern: String, path: String): Boolean = PathMatcher(pattern)
    .matches(Paths.get(path))

  /** Returns the exception that the matcher of the pattern throws. */
  private def error(matcher: => PathMatcher): String = Try(matcher) match {
    case _: Success[PathMatcher] => "no exception"
    case f: Failure[PathMatcher] => f.exception.toString
  }

  test("glob:**.scala") {
    assertEquals(matches("glob:**.scala", "a/b/c.scala"), true)
    assertEquals(matches("glob:**.scala", "c.scala"), true)
    assertEquals(matches("glob:**.scala", "c.sc"), false)
  }

  test("glob:*.scala") {
    assertEquals(matches("glob:*.scala", "c.scala"), true)
    assertEquals(matches("glob:*.scala", "b/c.scala"), false)
  }

  test("glob:a?c") {
    assertEquals(matches("glob:a?c", "abc"), true)
    assertEquals(matches("glob:a?c", "abbc"), false)
  }

  test("glob:*.{scala,sbt}") {
    assertEquals(matches("glob:*.{scala,sbt}", "c.sbt"), true)
    assertEquals(matches("glob:*.{scala,sbt}", "c.sc"), false)
  }

  test("regex:foo")(assertEquals(matches("regex:foo", "a/foo/b"), isJS))

  test("PathMatcher.Regex")(
    assertEquals(PathMatcher.Regex("foo").matches(Paths.get("a/foo/b")), true),
  )

  test("a pattern without a prefix") {
    val expected =
      if (isJS) "Invalid path matcher pattern: foo; no path matcher prefix"
      else "Invalid path matcher pattern: foo"
    assertNoDiff(
      error(PathMatcher("foo")),
      "scala.meta.internal.io.InvalidPathPatternException: " + expected,
    )
  }

  test("a pattern with an unknown prefix") {
    val expected =
      if (isJS) "scala.meta.internal.io.InvalidPathPatternException: " +
        "Invalid path matcher pattern: glog:**.scala; 'glog' is not supported"
      else if (isNative) "java.lang.UnsupportedOperationException"
      else "java.lang.UnsupportedOperationException: Syntax 'glog' not recognized"
    assertNoDiff(error(PathMatcher("glog:**.scala")), expected)
  }

  test("a regex pattern that does not compile") {
    val expected =
      if (isJS)
        """|scala.meta.internal.io.InvalidPathPatternException: Invalid path patcher regex: /(/; Unclosed group near index 1
           |(
           |""".stripMargin
      else if (isNative)
        """|scala.meta.internal.io.InvalidPathPatternException: Invalid path matcher pattern: regex:(; Unclosed group near index 1
           |(
           | ^
           |""".stripMargin
      else
        """|scala.meta.internal.io.InvalidPathPatternException: Invalid path matcher pattern: regex:(; Unclosed group near index 1
           |(
           |""".stripMargin
    assertNoDiff(error(PathMatcher("regex:(")), expected)
  }

  test("PathMatcher.Regex of a regex that does not compile") {
    val expected =
      if (isNative)
        """|scala.meta.internal.io.InvalidPathPatternException: Invalid path patcher regex: /(/; Unclosed group near index 1
           |(
           | ^
           |""".stripMargin
      else
        """|scala.meta.internal.io.InvalidPathPatternException: Invalid path patcher regex: /(/; Unclosed group near index 1
           |(
           |""".stripMargin
    assertNoDiff(error(PathMatcher.Regex("(")), expected)
  }

  test("an invalid pattern")(intercept[IllegalArgumentException](PathMatcher("foo")))

  test("PathMatcher.fixSeparatorsInPathPattern") {
    val expected = if (PathIO.isWindows) "a\\\\b" else "a/b"
    assertEquals(PathMatcher.fixSeparatorsInPathPattern("a/b"), expected)
  }

  test("PathMatcher.inPathMatcherForm") {
    val expected =
      if (PlatformFlags.isNativeOnWindows) "a/b" else if (PathIO.isWindows) "a\\\\b" else "a/b"
    assertEquals(PathMatcher.inPathMatcherForm("a/b"), expected)
  }

}
