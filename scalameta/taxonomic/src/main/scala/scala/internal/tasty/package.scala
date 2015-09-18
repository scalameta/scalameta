package scala.meta.internal

import java.io._
import java.nio.charset.Charset
import java.security.MessageDigest
import scala.meta.Dialect
import scala.meta.Source
import scala.meta.inputs._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.semantic._
import org.scalameta.data._
import org.scalameta.debug._
import org.scalameta.show._

package object tasty {
  @data class SyntacticDigest(dialect: Dialect, hash: String)
  type SemanticSource = Source

  implicit class XtensionSyntacticDigestTree(source: Source) {
    def syntacticDigest: SyntacticDigest = {
      val dialect = source.dialect
      val text = source.position match {
        case Position.Range(content, _, _, _) => new String(content.chars)
        case _ => throw new TastyException("syntactically adhoc trees are not supported")
      }
      val sha1 = MessageDigest.getInstance("SHA-1")
      sha1.reset()
      sha1.update(text.getBytes("UTF-8"))
      val hashBytes = sha1.digest()
      SyntacticDigest(dialect, hashBytes.map(b => "%02X".format(b)).mkString)
    }
  }

  def toTasty(digest: SyntacticDigest, source: SemanticSource): Array[Byte] = {
    try {
      val pickler = new TastyPickler

      val syntacticBuf = new TastyBuffer(20)
      pickler.newSection("ScalametaSyntactic", syntacticBuf)
      val dialectBytes = digest.dialect.name.getBytes("UTF-8")
      val hashBytes = digest.hash.getBytes("UTF-8")
      syntacticBuf.writeNat(dialectBytes.length)
      syntacticBuf.writeBytes(dialectBytes, dialectBytes.length)
      syntacticBuf.writeNat(hashBytes.length)
      syntacticBuf.writeBytes(hashBytes, hashBytes.length)

      source.requireAttributed()

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

      if (Debug.tasty) println(s"successfully written TASTY: ${digest.dialect}, ${digest.hash}, ${source.show[TopLevel]}")
      pickler.assembleParts()
    } catch {
      case ex: TastyException => throw ex
      case ex: Exception => throw new TastyException("of a protocol error", ex)
    }
  }

  def fromTasty(tastyBlob: Array[Byte]): (SyntacticDigest, SemanticSource) = {
    class SyntacticUnpickler extends TastyUnpickler.SectionUnpickler[SyntacticDigest]("ScalametaSyntactic") {
      def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
        val dialectLength = reader.readNat()
        val dialectName = new String(reader.readBytes(dialectLength), Charset.forName("UTF-8"))
        val dialect = Dialect.forName(dialectName)
        val hashLength = reader.readNat()
        val hash = new String(reader.readBytes(hashLength), Charset.forName("UTF-8"))
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
      val digest = unpickler.unpickle(new SyntacticUnpickler) match {
        case Some(digest) => digest
        case _ => throw new UntastyException("no ScalametaSyntactic section was found")
      }
      val source = unpickler.unpickle(new SemanticUnpickler) match {
        case Some(source) => source
        case _ => throw new UntastyException("no ScalametaSemantic section was found")
      }
      source.requireAttributed()
      if (Debug.tasty) println(s"successfully loaded TASTY: ${digest.dialect}, ${digest.hash}, ${source.show[TopLevel]}")
      (digest, source)
    } catch {
      case ex: UntastyException => throw ex
      case ex: Exception => throw new UntastyException("of a protocol error", ex)
    }
  }

  implicit class XtensionTastyDebug(debug: org.scalameta.debug.Debug.type) {
    def tasty = sys.props("tasty.debug") != null || sys.props("artifact.debug") != null
  }
}
