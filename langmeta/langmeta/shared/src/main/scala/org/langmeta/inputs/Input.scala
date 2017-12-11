package org.langmeta.inputs

import java.nio.{file => nio}
import java.nio.charset.Charset
import org.langmeta.internal.inputs._
import org.langmeta.semanticdb.Symbol
import org.langmeta.io._

sealed trait Input extends Product with Serializable with InternalInput {
  def chars: Array[Char]
  def text: scala.Predef.String = new scala.Predef.String(chars)
  def syntax: scala.Predef.String
  def structure: scala.Predef.String
}

object Input {
  case object None extends Input {
    lazy val chars = new Array[Char](0)
    override def toString: scala.Predef.String = syntax
    def syntax: scala.Predef.String = "<none>"
    def structure: scala.Predef.String = "Input.None"
  }

  final case class String(value: scala.Predef.String) extends Input {
    lazy val chars = value.toArray
    override def toString: scala.Predef.String = syntax
    def syntax: scala.Predef.String = "<string>"
    def structure: scala.Predef.String = s"""Input.String("$value")"""
  }

  final case class Stream(stream: java.io.InputStream, charset: Charset) extends Input {
    lazy val chars = new scala.Predef.String(org.langmeta.internal.io.InputStreamIO.readBytes(stream), charset).toArray
    protected def writeReplace(): AnyRef = new Stream.SerializationProxy(this)
    override def toString: scala.Predef.String = syntax
    def syntax: scala.Predef.String = "<stream>"
    def structure: scala.Predef.String = s"""Input.Stream(<stream>, Charset.forName("${charset.name}"))"""
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
      override def toString = s"""Proxy($orig)"""
    }
  }

  final case class File(path: AbsolutePath, charset: Charset) extends Input {
    lazy val chars = org.langmeta.internal.io.FileIO.slurp(path, charset).toArray
    protected def writeReplace(): AnyRef = new File.SerializationProxy(this)
    override def toString: scala.Predef.String = syntax
    def syntax: scala.Predef.String = path.syntax
    def structure: scala.Predef.String = s"""Input.File(new File("${path.syntax}"), Charset.forName("${charset.name}"))"""
  }
  object File {
    def apply(path: AbsolutePath): Input.File = apply(path, Charset.forName("UTF-8"))
    def apply(file: java.io.File, charset: Charset): Input.File = apply(AbsolutePath(file), charset)
    def apply(file: java.io.File): Input.File = apply(AbsolutePath(file))
    def apply(path: nio.Path, charset: Charset): Input.File = apply(AbsolutePath(path), charset)
    def apply(path: nio.Path): Input.File = apply(AbsolutePath(path))

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
    }
  }

  final case class VirtualFile(path: scala.Predef.String, value: scala.Predef.String) extends Input {
    lazy val chars = value.toArray
    def structure: scala.Predef.String = s"""Input.VirtualFile("$path", "$value")"""
    def syntax: scala.Predef.String = path
    override def toString: scala.Predef.String = syntax
  }

  final case class Synthetic(value: scala.Predef.String, input: Input, start: Int, end: Int) extends Input {
    lazy val chars = value.toCharArray
    def structure: scala.Predef.String = s"""Input.Synthetic("$value", ${input.structure}, $start, $end)"""
    def syntax: scala.Predef.String = "<synthetic>"
    override def toString: scala.Predef.String = syntax
  }

  final case class Denotation(value: scala.Predef.String, symbol: Symbol) extends Input {
    lazy val chars = value.toCharArray
    def structure: scala.Predef.String = s"""Input.Denotation("$value", "${symbol.structure}")"""
    def syntax: scala.Predef.String = "<denotation>"
    override def toString: scala.Predef.String = syntax
  }

  // NOTE: `start` and `end` are String.substring-style,
  // i.e. `start` is inclusive and `end` is not.
  // Therefore Slice.end can point to the last character of input plus one.
  final case class Slice(input: Input, start: Int, end: Int) extends Input {
    lazy val chars = input.chars.slice(start, end)
    def structure: scala.Predef.String = s"Input.Slice(${input.structure}, $start, $end)"
    def syntax: scala.Predef.String = "<slice>"
    override def toString: scala.Predef.String = syntax
  }
}
