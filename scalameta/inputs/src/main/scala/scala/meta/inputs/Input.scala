package scala.meta
package inputs

import java.nio.charset.Charset
import org.scalameta.data._
import org.scalameta.invariants._
import scala.meta.common._
import scala.meta.internal.inputs._

trait Input extends Optional with Product with Serializable with InternalInput {
  def chars: Array[Char]
}

object Input {
  @none object None extends Input {
    lazy val chars = new Array[Char](0)
    override def toString = "Input.None"
  }

  @data class String(s: scala.Predef.String) extends Input {
    lazy val chars = s.toArray
    override def toString = "Input.String(\"" + s + "\")"
  }

  @data class Stream(stream: java.io.InputStream, charset: Charset) extends Input {
    lazy val chars = scala.io.Source.fromInputStream(stream)(scala.io.Codec(charset)).mkString.toArray
    protected def writeReplace(): AnyRef = new Stream.SerializationProxy(this)
    override def toString = "Input.Stream(<stream>, Charset.forName(\"" + charset.name + "\"))"
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

  @data class File(file: java.io.File, charset: Charset) extends Input {
    lazy val chars = scala.io.Source.fromFile(file)(scala.io.Codec(charset)).mkString.toArray
    protected def writeReplace(): AnyRef = new File.SerializationProxy(this)
    override def toString = "Input.File(new File(\"" + file + "\"), Charset.forName(\"" + charset.name + "\"))"
  }
  object File {
    def apply(path: Predef.String): Input.File = new Input.File(new java.io.File(path), Charset.forName("UTF-8"))
    def apply(file: java.io.File): Input.File = new Input.File(file, Charset.forName("UTF-8"))
    def apply(file: java.io.File, charset: Charset): Input.File = new Input.File(file, charset)

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

  // NOTE: `start` and `end` are String.substring-style,
  // i.e. `start` is inclusive and `end` is not.
  // Therefore Slice.end can point to the last character of input plus one.
  @data class Slice(input: Input, start: Int, end: Int) extends Input {
    lazy val chars = input.chars.slice(start, end)
    override def toString = s"Input.Slice($input, $start, $end)"
  }

  implicit val charsToInput: Convert[Array[Char], Input] = Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert(Input.String(_))
  implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] = Convert(is => Input.Stream(is, Charset.forName("UTF-8")))
  implicit val fileToInput: Convert[java.io.File, Input] = Convert(f => Input.File(f, Charset.forName("UTF-8")))
}

private[meta] trait InputLiftables {
  val c: scala.reflect.macros.blackbox.Context
  import c.universe._
  implicit lazy val liftInput: Liftable[Input] = Liftable[Input] { input =>
    q"_root_.scala.meta.inputs.Input.String(${new String(input.chars)})"
  }
}
