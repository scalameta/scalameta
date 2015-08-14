package scala.meta.internal

import java.io._
import java.nio.charset.Charset
import java.security.MessageDigest
import scala.meta.Dialect
import scala.meta.Source
import scala.meta.syntactic.Content
import org.scalameta.data._

package object tasty {
  implicit class XtensionTastyWrite(source: SemanticSource) {
    def toTasty: Array[Byte] = {
      try {
        val pickler = new TastyPickler

        val syntacticBuf = new TastyBuffer(20)
        pickler.newSection("ScalametaSyntactic", syntacticBuf)
        val dialectBytes = source.dialect.name.getBytes("UTF-8")
        val text = source.position.input match { case input: Content => new String(input.chars); case _ => "" }
        val sha1 = MessageDigest.getInstance("SHA-1")
        sha1.reset()
        sha1.update(text.getBytes("UTF-8"))
        val hashBytes = sha1.digest()
        syntacticBuf.writeNat(dialectBytes.length)
        syntacticBuf.writeBytes(dialectBytes, dialectBytes.length)
        syntacticBuf.writeNat(hashBytes.length)
        syntacticBuf.writeBytes(hashBytes, hashBytes.length)

        val baos = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(baos)
        oos.writeObject(source)
        oos.close()
        baos.close()
        val semanticBlob = baos.toByteArray()
        val semanticBuf = new TastyBuffer(semanticBlob.length)
        pickler.newSection("ScalametaSemantic", semanticBuf)
        semanticBuf.writeNat(semanticBlob.length)
        semanticBuf.writeBytes(semanticBlob, semanticBlob.length)

        pickler.assembleParts()
      } catch {
        case ex: TastyException => throw ex
        case ex: Exception => throw new TastyException("of a protocol error", ex)
      }
    }
  }

  @data class SyntacticDigest(dialect: Dialect, hash: String)
  type SemanticSource = Source

  implicit class XtensionTastyRead(dummy: Source.type) {
    def fromTasty(tastyBlob: Array[Byte]): (SyntacticDigest, SemanticSource) = {
      class SyntacticUnpickler extends TastyUnpickler.SectionUnpickler[SyntacticDigest]("ScalametaSyntactic") {
        def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
          val dialectLength = reader.readNat()
          val dialectName = new String(reader.readBytes(dialectLength), Charset.forName("UTF-8"))
          val dialect = Dialect.forName(dialectName)
          val hashLength = reader.readNat()
          val hashBytes = reader.readBytes(hashLength)
          val hash = hashBytes.map(b => "%02X".format(b)).mkString
          SyntacticDigest(dialect, hash)
        }
      }

      class SemanticUnpickler extends TastyUnpickler.SectionUnpickler[SemanticSource]("ScalametaSemantic") {
        def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
          val sourceLength = reader.readNat()
          val sourceBlob = reader.readBytes(sourceLength)
          val bais = new ByteArrayInputStream(sourceBlob)
          val ois = new ObjectInputStream(bais)
          ois.readObject.asInstanceOf[SemanticSource]
        }
      }

      try {
        val unpickler = new TastyUnpickler(tastyBlob)
        val syntacticDigest = unpickler.unpickle(new SyntacticUnpickler) match {
          case Some(syntacticDigest) => syntacticDigest
          case _ => throw new UntastyException("no ScalametaSyntactic section was found")
        }
        val semanticSource = unpickler.unpickle(new SemanticUnpickler) match {
          case Some(semanticSource) => semanticSource
          case _ => throw new UntastyException("no ScalametaSemantic section was found")
        }
        (syntacticDigest, semanticSource)
      } catch {
        case ex: UntastyException => throw ex
        case ex: Exception => throw new UntastyException("of a protocol error", ex)
      }
    }
  }

  implicit class XtensionTastyDebug(debug: org.scalameta.debug.Debug.type) {
    def tasty = sys.props("tasty.debug") != null || sys.props("artifact.debug") != null
  }
}
