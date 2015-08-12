package scala.meta.internal

import java.io._
import java.security.MessageDigest
import scala.meta.Source
import scala.meta.syntactic.Content

package object tasty {
  implicit class XtensionTastyWriteTree(source: Source) {
    def toTasty: Array[Byte] = {
      val pickler = new TastyPickler

      val syntacticBuf = new TastyBuffer(20)
      pickler.newSection("ScalametaSyntactic", syntacticBuf)
      val text = source.position.input match { case input: Content => new String(input.chars); case _ => "" }
      val sha1 = MessageDigest.getInstance("SHA-1")
      sha1.reset()
      sha1.update(text.getBytes("UTF-8"))
      val digest = sha1.digest()
      syntacticBuf.writeNat(digest.length)
      syntacticBuf.writeBytes(digest, digest.length)

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
    }
  }

  implicit class XtensionTastyReadTree(dummy: Source.type) {
    def fromTasty(tastyBlob: Array[Byte]): Option[(String, Source)] = {
      if (tastyBlob.length == 0) return None
      val unpickler = new TastyUnpickler(tastyBlob)
      class SyntacticUnpickler extends TastyUnpickler.SectionUnpickler[String]("ScalametaSyntactic") {
        def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
          val length = reader.readNat()
          val bytes = reader.readBytes(length)
          bytes.map(b => "%2X".format(b)).mkString
        }
      }
      class SemanticUnpickler extends TastyUnpickler.SectionUnpickler[Array[Byte]]("ScalametaSemantic") {
        def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
          val length = reader.readNat()
          reader.readBytes(length)
        }
      }
      val syntacticDigest = unpickler.unpickle(new SyntacticUnpickler)
      syntacticDigest.flatMap(syntacticDigest => {
        val semanticBlob = unpickler.unpickle(new SemanticUnpickler)
        semanticBlob.flatMap(semanticBlob => {
          val bais = new ByteArrayInputStream(semanticBlob)
          val ois = new ObjectInputStream(bais)
          val semanticSource = ois.readObject.asInstanceOf[Source]
          Some((syntacticDigest, semanticSource))
        })
      })
    }
  }
}
