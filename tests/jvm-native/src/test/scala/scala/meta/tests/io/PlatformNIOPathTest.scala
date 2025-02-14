package scala.meta.tests.io

import java.io.File

class PlatformNIOPathTest extends NIOPathTestShared {

  private val pathsWithNames =
    if (File.separatorChar == '\\') Seq(
      ("//foo/bar//baz/", "\\\\foo\\bar\\", 1, Seq("baz")),
      ("//foo/bar///baz", "\\\\foo\\bar\\", 1, Seq("baz")),
      ("/foo/bar//baz/", "\\", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar///baz", "\\", 3, Seq("foo", "bar", "baz")),
      ("foo/bar////baz", null, 3, Seq("foo", "bar", "baz")),
      ("c:/foo/bar///baz", "c:\\", 3, Seq("foo", "bar", "baz")),
      ("c:\\foo\\\\bar\\baz", "c:\\", 3, Seq("foo", "bar", "baz")),
      ("foo\\bar\\baz", null, 3, Seq("foo", "bar", "baz")),
      ("\\\\foo\\bar\\baz", "\\\\foo\\bar\\", 1, Seq("baz"))
    )
    else Seq(
      ("//foo/bar//baz/", "/", 3, Seq("foo", "bar", "baz")),
      ("//foo/bar///baz", "/", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar/baz/", "/", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar/baz", "/", 3, Seq("foo", "bar", "baz")),
      ("foo/bar////baz", null, 3, Seq("foo", "bar", "baz"))
    )
  pathsWithNames.foreach((declareGetNameTest _).tupled)

}
