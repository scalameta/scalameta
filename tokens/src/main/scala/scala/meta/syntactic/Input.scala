package scala.meta
package syntactic

import java.nio.charset.Charset
import org.scalameta.adt._
import org.scalameta.convert._
import org.scalameta.invariants._
import scala.collection.{immutable, mutable}

// TODO: Input is really sealed, with the only two direct subclasses being Content and Tokens
// however, I don't really feel like mixing all the three concepts in a single file
trait Input extends Serializable {
  def tokens(implicit dialect: Dialect): Tokens
}

object Input {
  final case class String(s: scala.Predef.String) extends Content {
    lazy val chars = s.toArray
  }
  final case class File(f: java.io.File, charset: Charset) extends Content {
    lazy val chars = scala.io.Source.fromFile(f)(scala.io.Codec(charset)).mkString.toArray
    private def writeObject(out: java.io.ObjectOutputStream) = { out.writeObject(f); out.writeObject(charset.name) }
    private def readObject(in: java.io.ObjectInputStream) = { File(in.readObject.asInstanceOf[java.io.File], Charset.forName(in.readObject.asInstanceOf[Predef.String])) }
  }
  object File {
    def apply(path: Predef.String): Input.File = Input.File(new java.io.File(path))
    def apply(f: java.io.File): Input.File = Input.File(f, Charset.forName("UTF-8"))
  }
  implicit val charsToInput: Convert[Array[Char], Input] = Convert.apply(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert.apply(Input.String(_))
  implicit val fileToInput: Convert[java.io.File, Input] = Convert.apply(f => Input.File(f, Charset.forName("UTF-8")))
}
