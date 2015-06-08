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
      pickler.newSection("META", buf)
      buf.writeNat(metaBlob.length)
      metaBlob.foreach(b => buf.writeByte(b.toInt))
      pickler.assembleParts()
    }
  }
  implicit class XtensionTastyReadTree(dummy: Source.type) {
    def fromTasty(tastyBlob: Array[Byte]): Source = {
      // TODO: implement this
      null
    }
  }
}
