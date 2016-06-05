package scala.meta

import java.text._
import scala.util.Try
import scalatags.Text.all._
import org.scalameta.os._
import scala.compat.Platform.EOL
import scala.tools.nsc.interpreter._
import scala.tools.nsc.{Settings, MainGenericRunner}

object Readme {
  import scalatex.Main._

  def url(src: String) = a(href := src, src)

  private def unindent(frag: String): String = {
    // code frags are passed in raw from *.scalatex.
    val toStrip =
      " " * Try(frag.lines
            .withFilter(_.nonEmpty)
            .map(_.takeWhile(_ == ' ').length)
            .min).getOrElse(0)
    frag.lines.map(_.stripPrefix(toStrip)).mkString("\n")
  }

  private def executeInRepl(code: String): String = {
    case class RedFlag(pattern: String, directive: String, message: String)
    val redFlags = List(
      RedFlag("Abandoning crashed session.", "compilation crash", "crash in repl invocation"),
      RedFlag("error:", "compilation error", "compilation error in repl invocation"),
      RedFlag("Exception", "runtime exception", "runtime exception in repl invocation"),
      RedFlag("Error", "runtime exception", "runtime exception in repl invocation")
    )
    def validatePrintout(printout: String): Unit = {
      redFlags.foreach{ case RedFlag(pat, directive, msg) =>
        if (printout.contains(pat) && !code.contains("// " + directive)) {
          sys.error(msg + ": " + printout)
        }
      }
    }
    val s = new Settings
    s.Xnojline.value = true
    s.usejavacp.value = false
    s.classpath.value = sys.props("sbt.paths.readme.runtime.classes")
    val postprocessedCode = redFlags.foldLeft(code)((acc, curr) => acc.replace("// " + curr.directive, ""))
    val lines = ILoop.runForTranscript(postprocessedCode, s).lines.toList
    validatePrintout(lines.mkString(EOL))
    lines.drop(3).dropRight(2).map(_.replaceAll("\\s+$","")).mkString(EOL).trim
  }

  /**
    * repl session, inspired by tut.
    *
    * Example: code="1 + 1" returns
    * "scala> 1 + 1
    * res0: Int = 2"
    */
  def repl(code: String) = {
    hl.scala(executeInRepl(unindent(code)))
  }

  /**
    * repl session that has an invisible "import scala.meta._" attached to it.
    */
  def meta(code0: String) = {
    val code1 = s"import scala.meta._$EOL${unindent(code0).trim}"
    val result0 = executeInRepl(code1)
    val result1 = result0.split(EOL).drop(3).mkString(EOL)
    hl.scala(result1)
  }

  def note = b("NOTE")

  def repo: String = "https://github.com/scalameta/scalameta"

  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")

  def issues(ids: Int*) = span(ids.map(issue): _*)

  def half(frags: Frag*) = div(frags, width := "50%", float.left)

  def pairs(frags: Frag*) = div(frags, div(clear := "both"))

  def sideBySide(left: String, right: String) =
    pairs(List(left, right).map(x => half(hl.scala(x))): _*)

  def versionString = {
    val stdout = shell.check_output(s"git tag -l v*")
    val latestTag = stdout.split(EOL).last
    latestTag.stripPrefix("v")
  }

  def versionBadge = {
    def timestampOfTag(tag: String): String = {
      val stdout = shell.check_output(s"git show $tag --pretty=%aD")
      val original_dateOfTag = stdout.split(EOL).last
      val rfc2822 = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z")
      val dateOfTag = rfc2822.parse(original_dateOfTag)
      val pretty = new SimpleDateFormat("dd MMM yyyy")
      val pretty_dateOfTag = pretty.format(dateOfTag)
      s" (released on $pretty_dateOfTag)"
    }
    val version = versionString
    val timestamp = timestampOfTag("v" + version)
    version + timestamp
  }
}
