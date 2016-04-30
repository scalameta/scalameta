package scala.meta
package inputs

import java.nio.charset.Charset
import org.scalameta.data._
import org.scalameta.invariants._
import scala.collection.{immutable, mutable}
import scala.meta.common._

trait Input extends Serializable {
  def chars: Array[Char]

  // TODO: It's regrettable that we need to taint the pure abstraction of Input.
  // However, as #334 shows, we just can't redo offset -> line conversions over and over again.
  // This means that we gotta cache, and this instance is really the only place versatile enough.
  private lazy val lineIndices: Array[Int] = {
    val chars = this.chars
    val buf = new scala.collection.mutable.ArrayBuffer[Int]
    buf += 0
    var i = 0
    while (i < chars.length) {
      if (chars(i) == '\n') buf += (i + 1)
      i += 1
    }
    if (buf.last != chars.length) buf += chars.length // sentinel value used for binary search
    buf.toArray
  }

  private[meta] def lineToOffset(line: Int): Int = {
    // NOTE: The length-1 part is not a typo, it's to accommodate the sentinel value.
    if (!(0 <= line && line <= lineIndices.length - 1)) {
      val message = s"$line is not a valid line number, allowed [0..${lineIndices.length - 1}]"
      throw new IllegalArgumentException(message)
    }
    lineIndices(line)
  }

  private[meta] def offsetToLine(offset: Int): Int = {
    val chars = this.chars
    val a = lineIndices
    // NOTE: We allow chars.length, because it's a valid value for an offset.
    if (!(0 <= offset && offset <= chars.length)) {
      val message = s"$offset is not a valid offset, allowed [0..${chars.length}]"
      throw new IllegalArgumentException(message)
    }
    // NOTE: chars.length requires a really ugly special case.
    // If the file doesn't end with \n, then it's simply last_line:last_col+1.
    // But if the file does end with \n, then it's last_line+1:0.
    if (offset == chars.length && (0 < chars.length && chars(offset - 1) == '\n')) {
      return a.length - 1
    }
    var lo = 0
    var hi = a.length - 1
    while (hi - lo > 1) {
      val mid = (hi + lo) / 2
      if (offset < a(mid)) hi = mid
      else if (a(mid) == offset) return mid
      else /* if (a(mid) < offset */ lo = mid
    }
    return lo
  }
}

object Input {
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

  @data class Slice(Input: Input, from: Int, until: Int) extends Input {
    lazy val chars = Input.chars.slice(from, until)
    override def toString = s"Input.Slice($Input, $from, $until)"
  }

  implicit val charsToInput: Convert[Array[Char], Input] = Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert(Input.String(_))
  implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] = Convert(is => Input.Stream(is, Charset.forName("UTF-8")))
  implicit val fileToInput: Convert[java.io.File, Input] = Convert(f => Input.File(f, Charset.forName("UTF-8")))
}
