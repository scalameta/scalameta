package scala.meta
package inputs

import java.nio.charset.Charset
import org.scalameta.convert._
import scala.meta.tokens._
import scala.meta.tokenizers.common._

trait Content extends Input {
  def chars: Array[Char]

  private val tokenCache = scala.collection.mutable.Map[Dialect, Tokens]()
  final override def tokens(implicit dialect: Dialect, tokenize: Tokenize): Tokens = {
    tokenCache.getOrElseUpdate(dialect, tokenize(this))
  }
}

object Content {
  // TODO: Deduplicate wrt the implicits in Input.
  // I tried moving them to a shared trait, but then scalac produces super weird error messages like:
  //  [error]                     ^
  //  [error] /Users/xeno_by/Projects/core/scalameta/tokenquasiquotes/src/main/scala/scala/meta/internal/tokenquasiquotes/ReificationMacros.scala:68: ambiguous implicit values:
  //  [error]  both method stringToInput in trait InputConverters of type [T >: scala.meta.syntactic.Content <: scala.meta.syntactic.Input]=> org.scalameta.convert.Convert[String,T]
  //  [error]  and method stringToInput in trait InputConverters of type [T >: scala.meta.syntactic.Content <: scala.meta.syntactic.Input]=> org.scalameta.convert.Convert[String,T]
  //  [error]  match expected type org.scalameta.convert.Convert[String,scala.meta.syntactic.Content]
  implicit val charsToInput: Convert[Array[Char], Content] = Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Content] = Convert(Input.String(_))
  implicit val fileToInput: Convert[java.io.File, Content] = Convert(f => Input.File(f, Charset.forName("UTF-8")))
}
