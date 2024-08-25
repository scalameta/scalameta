package scala.meta.inputs

import scala.meta.common._
import scala.meta.internal._
import scala.meta.io._
import scala.meta.tokenizers.TokenizerOptions

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.nio.{file => nio}

sealed trait Input extends Product with Serializable with inputs.InternalInput {
  def chars: Array[Char]
  def text: String = new String(chars)

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
  }

  final case class String(value: Predef.String) extends Text {
    override def toString = s"""Input.String("$value")"""
  }

  final case class Stream(stream: java.io.InputStream, charset: Charset) extends Text {
    override protected lazy val value: Predef.String =
      new Predef.String(io.InputStreamIO.readBytes(stream), charset)
    protected def writeReplace(): AnyRef = new Stream.SerializationProxy(this)
    override def toString = s"""Input.Stream(<stream>, Charset.forName("${charset.name}"))"""
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
    override def toString =
      s"""Input.File(new File("${path.syntax}"), Charset.forName("${charset.name}"))"""
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
    override def toString = s"""Input.VirtualFile("$path", "$value")"""
  }

  // NOTE: `start` and `end` are String.substring-style,
  // i.e. `start` is inclusive and `end` is not.
  // Therefore Slice.end can point to the last character of input plus one.
  final case class Slice(input: Input, start: Int, end: Int) extends Text {
    override protected lazy val value: Predef.String = input.text.substring(start, end)
    override def toString = s"Input.Slice($input, $start, $end)"
  }

  final case class Ammonite(input: Input) extends Proxy {
    override def toString = s"Input.Ammonite($input)"
  }

  final case class WithTokenizerOptions private[meta] (input: Input, options: TokenizerOptions)
      extends Proxy {
    override def toString = s"Input.WithTokenizerOptions($input, $tokenizerOptions)"
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
