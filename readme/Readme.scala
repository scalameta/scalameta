package scala.meta

import java.text._
import scala.util.Try
import scalatags.Text.all._
import com.twitter.util.Eval
import org.scalameta.os._
import scala.compat.Platform.EOL

object Readme {
  import scalatex.Main._

  val eval = new Eval()

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

  /**
    * repl session, inspired by tut.
    *
    * Example: code="1 + 1" returns
    * "scala> 1 + 1
    * res0: Int = 2"
    */
  def repl(code: String) = {
    val expressions = s"{$code}".parse[Stat].get.asInstanceOf[Term.Block].stats
    val evaluated = eval[Any](code)
    val output = evaluated match {
      case s: String =>
        s"""
           |"$s"""".stripMargin
      case x => x.toString
    }
    val result = s"""${expressions
                      .map(x => s"scala> ${x.toString().trim}")
                      .mkString("\n")}
                    |res0: ${evaluated.getClass.getName} = $output
                    |""".stripMargin
    hl.scala(result)
  }

  def note = b("NOTE")

  def repo: String = "https://github.com/scalameta/scalameta"

  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")

  def issues(ids: Int*) = span(ids.map(issue): _*)

  def half(frags: Frag*) = div(frags, width := "50%", float.left)

  def pairs(frags: Frag*) = div(frags, div(clear := "both"))

  def sideBySide(left: String, right: String) =
    pairs(List(left, right).map(x => half(hl.scala(x))): _*)

  def versionBadge = {
    def timestampOfTag(tag: String): String = {
      shell.call(s"git fetch origin refs/tags/$tag:refs/tags/$tag")
      val stdout = shell.check_output(s"git show $tag --pretty=%aD")
      val original_dateOfTag = stdout.split(EOL).last
      val rfc2822 = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z")
      val dateOfTag = rfc2822.parse(original_dateOfTag)
      val pretty = new SimpleDateFormat("dd MMM yyyy")
      val pretty_dateOfTag = pretty.format(dateOfTag)
      s" (released on $pretty_dateOfTag)"
    }
    val version = Versions.stable
    val timestamp = timestampOfTag("v" + version)
    version + timestamp
  }
}
