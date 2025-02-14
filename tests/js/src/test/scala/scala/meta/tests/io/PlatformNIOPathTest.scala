package scala.meta.tests.io

import java.io.File

class PlatformNIOPathTest extends NIOPathTestShared {

  private val pathsWithNames =
    if (File.separatorChar == '\\') Seq(
      ("//foo/bar//baz/", "\\", 1, Nil),
      ("//foo/bar///baz", "\\", 1, Nil),
      ("/foo/bar//baz/", "\\", 1, Nil),
      ("/foo/bar///baz", "\\", 1, Nil),
      ("foo/bar////baz", null, 1, Seq("foo/bar////baz")),
      ("c:/foo/bar///baz", "\\", 1, Nil),
      ("c:\\foo\\\\bar\\baz", "\\", 3, Seq("foo", "", "bar", "baz")),
      ("foo\\bar\\baz", null, 3, Seq("foo", "bar", "baz")),
      ("\\\\foo\\bar\\baz", "\\", 3, Seq("", "foo", "bar", "baz"))
    )
    else Seq(
      ("//foo/bar//baz/", "/", 3, Seq("", "foo", "bar", "", "baz")),
      ("//foo/bar///baz", "/", 3, Seq("", "foo", "bar", "", "", "baz")),
      ("/foo/bar/baz/", "/", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar/baz", "/", 3, Seq("foo", "bar", "baz")),
      ("foo/bar////baz", null, 3, Seq("foo", "bar", "", "", "", "baz"))
    )
  pathsWithNames.foreach((declareGetNameTest _).tupled)

}
