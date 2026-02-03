package scala.meta.inputs

import scala.meta.common._
import scala.meta.internal._
import scala.meta.io._
import scala.meta.tokenizers.TokenizerOptions

import java.lang.{StringBuilder => JSB}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.{file => nio}

sealed trait Input extends Product with Serializable with inputs.InternalInput {
  def chars: Array[Char]
  def text: String = new String(chars)

  private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit

  private[meta] def tokenizerOptions: TokenizerOptions = implicitly[TokenizerOptions]
  def withoutTokenizerOptions: Input = this
  def withTokenizerOptions(implicit options: TokenizerOptions): Input =
    if (options eq null) this else Input.WithTokenizerOptions(this, options)
}

object Input {

  sealed trait Text extends Input {
    protected val value: Predef.String
    override def text: Predef.String = value
    override lazy val chars: Array[Char] = value.toCharArray
  }

  sealed trait Proxy extends Input {
    val input: Input
    override def chars: Array[Char] = input.chars
    override def text: Predef.String = input.text
    override private[meta] def tokenizerOptions: TokenizerOptions = input.tokenizerOptions
  }

  case object None extends Text {
    protected val value = ""
    override def toString = "Input.None"
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = sb.append(toString)
  }

  final case class String(value: Predef.String) extends Text {
    override def toString = s"""str($value)"""
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = {
      sb.append("str")
      getStringSlice(value, beg, end)
    }
  }

  private def getStringSlice(value: Predef.String, beg: Int, end: Int)(implicit sb: JSB): Unit = {
    val begAdj = 0.max(beg - 10)
    val endAdj = value.length.min(end + 10)
    sb.append('(')
    if (begAdj > 0) sb.append("... ")
    sb.append(value, begAdj, endAdj)
    if (endAdj < value.length) sb.append(" ...")
    sb.append(')')
  }

  final case class Stream(stream: java.io.InputStream, charset: Charset) extends Text {
    override protected lazy val value: Predef.String =
      new Predef.String(io.InputStreamIO.readBytes(stream), charset)
    protected def writeReplace(): AnyRef = new Stream.SerializationProxy(this)
    override def toString = s"""stream[${charset.name}]"""
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = sb.append(toString)
  }
  object Stream {
    @SerialVersionUID(1L)
    private class SerializationProxy(
        @transient
        private var orig: Stream
    ) extends Serializable {
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
      override def toString = s"""Proxy($orig)"""
    }
  }

  final case class File(path: AbsolutePath, charset: Charset) extends Text {
    override protected lazy val value: Predef.String = io.FileIO.slurp(path, charset)
    protected def writeReplace(): AnyRef = new File.SerializationProxy(this)
    override def toString = s"""file:${path.syntax}[${charset.name}]"""
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = sb.append(toString)
  }
  object File {
    def apply(path: AbsolutePath): Input.File = apply(path, Charset.forName("UTF-8"))
    def apply(file: java.io.File, charset: Charset): Input.File = apply(AbsolutePath(file), charset)
    def apply(file: java.io.File): Input.File = apply(AbsolutePath(file))
    def apply(path: nio.Path, charset: Charset): Input.File = apply(AbsolutePath(path), charset)
    def apply(path: nio.Path): Input.File = apply(AbsolutePath(path))

    @SerialVersionUID(1L)
    private class SerializationProxy(
        @transient
        private var orig: File
    ) extends Serializable {
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
      override def toString = s"""Proxy($orig)"""
    }
  }

  final case class VirtualFile(path: Predef.String, value: Predef.String) extends Text {
    override def toString = s"""virtfile:$path($value)"""
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = {
      sb.append("virtfile:").append(path)
      getStringSlice(value, beg, end)
    }
  }

  // NOTE: `start` and `end` are String.substring-style,
  // i.e. `start` is inclusive and `end` is not.
  // Therefore Slice.end can point to the last character of input plus one.
  final case class Slice(input: Input, start: Int, end: Int) extends Text {
    override protected lazy val value: Predef.String = input.text.substring(start, end)
    override def toString = {
      implicit val sb = new JSB
      getInputSlice(input, start, end)
      sb.toString
    }
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = input
      .toStringSlice(start + beg, start + end)
  }

  private[meta] def getInputSlice(input: Input, beg: Int, end: Int)(implicit sb: JSB): Unit = {
    sb.append('[').append(beg).append(',').append(end).append(") in ")
    input.toStringSlice(beg, end)
  }

  final case class Ammonite(input: Input) extends Proxy {
    override def toString = "ammonite:" + input.toString
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = {
      sb.append("ammonite:")
      input.toStringSlice(beg, end)
    }
  }

  final case class WithTokenizerOptions private[meta] (input: Input, options: TokenizerOptions)
      extends Proxy {
    override def toString = input.toString
    private[meta] def toStringSlice(beg: Int, end: Int)(implicit sb: JSB): Unit = input
      .toStringSlice(beg, end)
    override private[meta] def tokenizerOptions = options
    override def withoutTokenizerOptions: Input = input
    override def withTokenizerOptions(implicit options: TokenizerOptions): Input =
      if (options eq this.options) this else if (options eq null) input else copy(options = options)
  }

  implicit val charsToInput: Convert[Array[Char], Input] =
    Convert(chars => Input.String(new Predef.String(chars)))
  implicit val stringToInput: Convert[Predef.String, Input] = Convert(Input.String)
  implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] =
    Convert(is => Input.Stream(is, StandardCharsets.UTF_8))
  // NOTE: fileToInput is lazy to avoid linking errors in Scala.js
  implicit lazy val fileToInput: Convert[java.io.File, Input] = Convert(Input.File.apply)
  implicit lazy val nioPathToInput: Convert[java.nio.file.Path, Input] = Convert(Input.File.apply)
  implicit lazy val absolutePathToInput: Convert[AbsolutePath, Input] = Convert(Input.File.apply)
}
