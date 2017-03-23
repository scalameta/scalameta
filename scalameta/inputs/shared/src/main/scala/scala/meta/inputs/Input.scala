package scala.meta
package inputs

import java.nio.charset.Charset

import org.scalameta.adt.{Liftables => AdtLiftables}
import org.scalameta.data._
import org.scalameta.invariants._
import scala.meta.common._
import scala.meta.internal.inputs._
import scala.meta.io._

trait Input extends Optional with Product with Serializable with InternalInput {
  def location = "<input>"
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
    lazy val chars = InputStreamIO.read(stream, charset).toArray
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

  @data class File(path: AbsolutePath, charset: Charset) extends Input {
    @deprecated("Use .path instead", "1.8.0")
    def file: java.io.File = new java.io.File(path.absolute)
    override def location = s"<${path.absolute}>"
    lazy val chars = path.read.toArray
    protected def writeReplace(): AnyRef = new File.SerializationProxy(this)
    override def toString = "Input.File(new File(\"" + path.relative + "\"), Charset.forName(\"" + charset.name + "\"))"
  }
  object File {
    def apply(path: AbsolutePath, charset: Charset): Input.File = new Input.File(path, charset)
    def apply(path: AbsolutePath): Input.File = apply(path, Charset.forName("UTF-8"))
    def apply(path: Predef.String): Input.File = apply(AbsolutePath(path))
    def apply(file: java.io.File): Input.File = apply(file.getAbsolutePath)
    def apply(file: java.io.File, charset: Charset): Input.File = apply(AbsolutePath(file), charset)

    @SerialVersionUID(1L) private class SerializationProxy(@transient private var orig: File) extends Serializable {
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        out.writeObject(orig.path)
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
  implicit lazy val fileToInput: Convert[java.io.File, Input] = Convert(f => Input.File(f, Charset.forName("UTF-8")))
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait InputLiftables extends AdtLiftables {
  implicit lazy val liftInput: u.Liftable[Input] = u.Liftable[Input] { input =>
    import u._
    val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
    q"_root_.scala.meta.inputs.Input.String(${new String(input.chars)})"
  }
}
