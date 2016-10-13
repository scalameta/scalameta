package scala.meta.internal

import java.io._
import java.nio.charset.Charset
import java.security.MessageDigest
import scala.meta.Dialect
import scala.meta.Source
import scala.meta.inputs._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.semantic._
import scala.meta.prettyprinters._
import org.scalameta.data._

package object tasty {
  def toTasty(source: Source): Array[Byte] = {
    try {
      // TODO: uncomment this when the converter works well
      // source.requireAttributed()

      val baos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(baos)
      oos.writeObject(source)
      oos.close()
      baos.close()
      val semanticBlob = baos.toByteArray()
      val semanticBuf = new TastyBuffer(semanticBlob.length)

      val pickler = new TastyPickler
      pickler.newSection("Scalameta", semanticBuf)
      semanticBuf.writeNat(semanticBlob.length)
      semanticBuf.writeBytes(semanticBlob, semanticBlob.length)

      logTasty(println(s"successfully written TASTY: ${source.show[TopLevel]}"))
      pickler.assembleParts()
    } catch {
      case ex: TastyException => throw ex
      case ex: NotImplementedError => throw new TastyException("of a protocol error", ex)
      case ex: Exception => throw new TastyException("of a protocol error", ex)
    }
  }

  def fromTasty(tastyBlob: Array[Byte]): Source = {
    class ScalametaUnpickler extends TastyUnpickler.SectionUnpickler[Source]("Scalameta") {
      def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
        val sourceLength = reader.readNat()
        val sourceBlob = reader.readBytes(sourceLength)
        val bais = new ByteArrayInputStream(sourceBlob)
        val ois = new ObjectInputStream(bais)
        ois.readObject.asInstanceOf[Source]
      }
    }

    try {
      val unpickler = new TastyUnpickler(tastyBlob)
      val source = unpickler.unpickle(new ScalametaUnpickler) match {
        case Some(source) => source
        case _ => throw new UntastyException("no Scalameta section was found")
      }
      // TODO: uncomment this when the converter works well
      // source.requireAttributed()
      logTasty(println(s"successfully loaded TASTY: ${source.show[TopLevel]}"))
      source
    } catch {
      case ex: UntastyException => throw ex
      case ex: NotImplementedError => throw new UntastyException("of a protocol error", ex)
      case ex: Exception => throw new UntastyException("of a protocol error", ex)
    }
  }

  def logTasty(op: => Unit): Unit = {
    if (sys.props("tasty.debug") != null) op
  }
}
