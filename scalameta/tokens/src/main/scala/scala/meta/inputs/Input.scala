package scala.meta
package inputs

import java.nio.charset.Charset
import org.scalameta.data._
import org.scalameta.convert._
import org.scalameta.invariants._
import scala.collection.{immutable, mutable}
import scala.meta.tokens._
import scala.meta.tokenizers.common._

// TODO: Input is really sealed, with the only two direct subclasses being Content and Tokens
// however, I don't really feel like mixing all the three concepts in a single file
trait Input extends Serializable {
  def tokens(implicit dialect: Dialect, tokenize: Tokenize): Tokens
}

object Input {
  @data class String(s: scala.Predef.String) extends Content {
    lazy val chars = s.toArray
    override def toString = "Input.Content(\"" + s + "\")"
  }

  @data class Stream(stream: java.io.InputStream, charset: Charset) extends Content {
    lazy val chars = scala.io.Source.fromInputStream(stream)(scala.io.Codec(charset)).mkString.toArray
    protected def writeReplace(): AnyRef = new Stream.SerializationProxy(this)
    override def toString = {
      if (charset.name == "UTF-8") "Input.Stream(<stream>)"
      else "Input.Stream(<stream>, Charset.forName(\"" + charset.name + "\"))"
    }
  }
  object Stream {
    @SerialVersionUID(1L) private class SerializationProxy(@transient private var orig: Stream) extends Serializable {
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        out.writeObject(orig.chars)
        out.writeObject(orig.charset.name)
      }
      private def readObject(in: java.io.ObjectInputStream): Unit = {
        val chars = in.readObject.asInstanceOf[Array[Char]]
        val charset = Charset.forName(in.readObject.asInstanceOf[Predef.String])
        val stream = new java.io.ByteArrayInputStream(new Predef.String(chars).getBytes(charset))
        orig = Stream(stream, charset)
      }
      private def readResolve(): AnyRef = orig
      override def toString = s"Proxy($orig)"
    }
  }

  @data class File(file: java.io.File, charset: Charset) extends Content {
    lazy val chars = scala.io.Source.fromFile(file)(scala.io.Codec(charset)).mkString.toArray
    protected def writeReplace(): AnyRef = new File.SerializationProxy(this)
    override def toString = {
      if (charset.name == "UTF-8") "Input.File(new File(\"" + file + "\"))"
      else "Input.File(new File(\"" + file + "\"), Charset.forName(\"" + charset.name + "\"))"
    }
  }
  object File {
    def apply(path: Predef.String): Input.File = Input.File(new java.io.File(path))
    def apply(file: java.io.File): Input.File = Input.File(file, Charset.forName("UTF-8"))

    @SerialVersionUID(1L) private class SerializationProxy(@transient private var orig: File) extends Serializable {
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        out.writeObject(orig.file)
        out.writeObject(orig.charset.name)
      }
      private def readObject(in: java.io.ObjectInputStream): Unit = {
        val file = in.readObject.asInstanceOf[java.io.File]
        val charset = Charset.forName(in.readObject.asInstanceOf[Predef.String])
        orig = File(file, charset)
      }
      private def readResolve(): AnyRef = orig
      override def toString = s"Proxy($orig)"
    }
  }

  implicit val charsToInput: Convert[Array[Char], Input] = Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert(Input.String(_))
  implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] = Convert(is => Input.Stream(is, Charset.forName("UTF-8")))
  implicit val fileToInput: Convert[java.io.File, Input] = Convert(f => Input.File(f, Charset.forName("UTF-8")))
}
