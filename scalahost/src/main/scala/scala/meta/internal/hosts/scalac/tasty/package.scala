package scala.meta.internal.hosts.scalac

import java.io._
import java.security.MessageDigest
import scala.meta.Source
import scala.meta.syntactic.Content

package object tasty {
  implicit class XtensionTastyWriteTree(source: Source) {
    def toTasty: Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(baos)
      oos.writeObject(source)
      oos.close()
      baos.close()
      val sourceBlob = baos.toByteArray()

      val pickler = new TastyPickler
      val sourceBuf = new TastyBuffer(sourceBlob.length)
      pickler.newSection("ScalametaSource", sourceBuf)
      sourceBuf.writeNat(sourceBlob.length)
      sourceBuf.writeBytes(sourceBlob, sourceBlob.length)

      val hashBuf = new TastyBuffer(20)
      pickler.newSection("ScalametaHash", hashBuf)
      val text = source.position.input match { case input: Content => new String(input.chars); case _ => "" }
      val sha1 = MessageDigest.getInstance("SHA-1")
      sha1.reset()
      sha1.update(text.getBytes("UTF-8"))
      val digest = sha1.digest()
      hashBuf.writeNat(digest.length)
      hashBuf.writeBytes(digest, digest.length)

      pickler.assembleParts()
    }
  }

  implicit class XtensionTastyReadTree(dummy: Source.type) {
    def fromTasty(tastyBlob: Array[Byte]): Option[(Source, String)] = {
      if (tastyBlob.length == 0) return None
      val unpickler = new TastyUnpickler(tastyBlob)
      class ArrayUnpickler(name: String) extends TastyUnpickler.SectionUnpickler[Array[Byte]](name) {
        def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
          val length = reader.readNat()
          reader.readBytes(length)
        }
      }
      class SourceUnpickler extends TastyUnpickler.SectionUnpickler[Array[Byte]]("ScalametaSource")
      class HashUnpickler extends TastyUnpickler.SectionUnpickler[Array[Byte]]("ScalametaHash")
      val sourceBlob = unpickler.unpickle(new SourceUnpickler)
      sourceBlob.flatMap(sourceBlob => {
        val bais = new ByteArrayInputStream(sourceBlob)
        val ois = new ObjectInputStream(bais)
        val source = ois.readObject.asInstanceOf[Source]
        val hash = unpickler.unpickle(new HashUnpickler)
        hash.map(hash => (source, hash.map(b => "%2X".format(b)).mkString))
      })
    }
  }
}
