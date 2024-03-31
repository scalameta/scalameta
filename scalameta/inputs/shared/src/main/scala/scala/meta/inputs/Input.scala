package scala.meta.inputs

import java.nio.{file => nio}
import java.nio.charset.Charset
import scala.meta.common._
import scala.meta.internal.inputs._
import scala.meta.io._

sealed trait Input extends Product with Serializable with InternalInput {
  def chars: Array[Char]
  def text: String = new String(chars)
}

object Input {
  case object None extends Input {
    lazy val chars = new Array[Char](0)
    override def toString = "Input.None"
  }

  final case class String(value: scala.Predef.String) extends Input {
    lazy val chars = value.toArray
    override def toString = s"""Input.String("$value")"""
  }

  final case class Stream(stream: java.io.InputStream, charset: Charset) extends Input {
    lazy val chars =
      new scala.Predef.String(scala.meta.internal.io.InputStreamIO.readBytes(stream), charset)
        .toArray
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

  final case class File(path: AbsolutePath, charset: Charset) extends Input {
    lazy val chars = scala.meta.internal.io.FileIO.slurp(path, charset).toArray
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

  final case class VirtualFile(path: scala.Predef.String, value: scala.Predef.String)
      extends Input {
    lazy val chars = value.toArray
    override def text: scala.Predef.String = value
    override def toString = s"""Input.VirtualFile("$path", "$value")"""
  }

  // NOTE: `start` and `end` are String.substring-style,
  // i.e. `start` is inclusive and `end` is not.
  // Therefore Slice.end can point to the last character of input plus one.
  final case class Slice(input: Input, start: Int, end: Int) extends Input {
    lazy val chars = input.chars.slice(start, end)
    override def toString = s"Input.Slice($input, $start, $end)"
  }

  final case class Ammonite(input: Input) extends Input {
    override def chars = input.chars
    override def text = input.text
    override def toString = s"Input.Ammonite($input)"
  }

  implicit val charsToInput: Convert[Array[Char], Input] =
    Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert(Input.String)
  implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] =
    Convert(is => Input.Stream(is, Charset.forName("UTF-8")))
  // NOTE: fileToInput is lazy to avoid linking errors in Scala.js
  implicit lazy val fileToInput: Convert[java.io.File, Input] = Convert(Input.File.apply)
  implicit lazy val nioPathToInput: Convert[java.nio.file.Path, Input] = Convert(Input.File.apply)
  implicit lazy val absolutePathToInput: Convert[AbsolutePath, Input] = Convert(Input.File.apply)
}
