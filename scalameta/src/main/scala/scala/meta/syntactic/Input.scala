package scala.meta
package syntactic

import java.nio.charset.Charset
import org.scalameta.adt._
import org.scalameta.convert._
import scala.collection.mutable
import scala.meta.internal.tokenizers._

trait Input {
  def content: Array[Char]
  private val tokenCache = mutable.Map[Dialect, Vector[Token]]()
  def tokens(implicit dialect: Dialect): Vector[Token] = tokenCache.getOrElseUpdate(dialect, tokenize(this))
}

object Input {
  final case object None extends Input {
    lazy val content = new Array[Char](0)
  }
  final case class String(s: scala.Predef.String) extends Input {
    lazy val content = s.toArray
  }
  final case class File(f: java.io.File, charset: Charset) extends Input {
    lazy val content = scala.io.Source.fromFile(f)(scala.io.Codec(charset)).mkString.toArray
  }
  object File {
    def apply(path: Predef.String): Input.File = Input.File(new java.io.File(path), Charset.forName("UTF-8"))
  }
  implicit val stringToOrigin: Convert[scala.Predef.String, Input] = Convert.apply(Input.String(_))
  implicit val fileToOrigin: Convert[java.io.File, Input] = Convert.apply(f => Input.File(f, Charset.forName("UTF-8")))
}
