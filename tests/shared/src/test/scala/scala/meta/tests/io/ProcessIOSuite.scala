package scala.meta.tests.io

import scala.meta.internal.io.PlatformFlags.isJS
import scala.meta.internal.io.ProcessIO
import scala.meta.io.AbsolutePath

import scala.util.{Failure, Success}

import munit.FunSuite

class ProcessIOSuite extends FunSuite {

  private val tmp = FunFixture[AbsolutePath](_ => TestTree.mkdtemp(), TestTree.delete)

  /** Returns the output lines, or the exception, with `<root>` in place of the root. */
  private def run(root: AbsolutePath, cmd: String*): String = {
    val res = ProcessIO.runArgv(cmd, Some(root.toNIO)) match {
      case s: Success[Seq[String]] => s.value.mkString("\n")
      case f: Failure[Seq[String]] => f.exception.toString
    }
    res.replace(root.toString, "<root>")
  }

  test("ProcessIO.runArgv of git --version") {
    val obtained = ProcessIO.runArgv(Seq("git", "--version"), None).map(_.toList)
    assert(obtained.toOption.exists(_.head.startsWith("git version ")), obtained)
  }

  tmp.test("ProcessIO.runArgv of git --version in a directory")(root =>
    assert(run(root, "git", "--version").startsWith("git version ")),
  )

  tmp.test("ProcessIO.runArgv of a command that exits non-zero") { root =>
    val expected =
      if (isJS)
        """|scala.scalajs.js.JavaScriptException: Failed to run 'git -C missing status'. Error:
           |fatal: cannot change to 'missing': No such file or directory
           |""".stripMargin
      else "java.lang.IllegalStateException: Failed to run 'git -C missing status'. Error: \n" +
        "> fatal: cannot change to 'missing': No such file or directory\n"
    assertNoDiff(run(root, "git", "-C", "missing", "status"), expected)
  }

  tmp.test("ProcessIO.runArgv of a missing executable") { root =>
    val obtained = run(root, "scalameta-missing-executable")
    if (isJS) assertNoDiff(obtained, "undefined")
    else assert(
      obtained.startsWith(
        "java.lang.IllegalStateException: Failed to run 'scalameta-missing-executable'. Error: ",
      ),
      obtained,
    )
  }

}
