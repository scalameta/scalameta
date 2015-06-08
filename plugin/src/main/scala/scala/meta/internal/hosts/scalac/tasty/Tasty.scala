package scala.meta
package internal.hosts.scalac

package object tasty {
  implicit class XtensionTastyWriteTree(source: Source) {
    def toTasty: Array[Byte] = {
      // TODO: implement this
      Array[Byte]()
    }
  }
  implicit class XtensionTastyReadTree(dummy: Source.type) {
    def fromTasty(blob: Array[Byte]): Source = {
      // TODO: implement this
      null
    }
  }
}
