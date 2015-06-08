package scala.meta
package internal.hosts.scalac

package object tasty {
  implicit class XtensionTastyWriteTree(source: Source) {
    def toTasty: Array[Byte] = {
      import java.io._
      val baos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(baos)
      oos.writeObject(source)
      oos.close()
      baos.close()
      val metaBlob = baos.toByteArray()
      val pickler = new TastyPickler
      val buf = new TastyBuffer(metaBlob.length)
      pickler.newSection("Scalameta", buf)
      buf.writeNat(metaBlob.length)
      buf.writeBytes(metaBlob, metaBlob.length)
      pickler.assembleParts()
    }
  }
  implicit class XtensionTastyReadTree(dummy: Source.type) {
    def fromTasty(tastyBlob: Array[Byte]): Option[Source] = {
      val unpickler = new TastyUnpickler(tastyBlob)
      class ScalametaUnpickler extends TastyUnpickler.SectionUnpickler[Array[Byte]]("Scalameta") {
        def unpickle(reader: TastyReader, tastyName: TastyName.Table) = {
          val length = reader.readNat()
          reader.readBytes(length)
        }
      }
      val metaBlob = unpickler.unpickle(new ScalametaUnpickler)
      metaBlob.map(metaBlob => {
        import java.io._
        val bais = new ByteArrayInputStream(metaBlob)
        val ois = new ObjectInputStream(bais)
        ois.readObject.asInstanceOf[Source]
      })
    }
  }
}
