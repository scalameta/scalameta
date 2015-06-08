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
      baos.toByteArray()
    }
  }
  implicit class XtensionTastyReadTree(dummy: Source.type) {
    def fromTasty(blob: Array[Byte]): Source = {
      // TODO: implement this
      null
    }
  }
}
