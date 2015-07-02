package org.scalameta.reflection

trait Metadata {
  self: MacroToolkit =>

  import global._
  import internal._
  import decorators._
  import scala.reflect.ClassTag
  import scala.reflect.macros.Attachments
  import org.scalameta.invariants._

  // NOTE: this boilerplate is unfortunately necessary, because we don't expose Attachable in the public API
  trait Attachable[-T] {
    def attachments(carrier: T): Attachments { type Pos = Position }
    def updateAttachment[U: ClassTag](carrier: T, attachment: U): Unit
  }
  object Attachable {
    implicit object TreeAttachable extends Attachable[Tree] {
      def attachments(carrier: Tree): Attachments { type Pos = Position } = carrier.attachments
      def updateAttachment[U: ClassTag](carrier: Tree, attachment: U): Unit = carrier.updateAttachment(attachment)
    }
    implicit object SymbolAttachable extends Attachable[Symbol] {
      def attachments(carrier: Symbol): Attachments { type Pos = Position } = carrier.attachments
      def updateAttachment[U: ClassTag](carrier: Symbol, attachment: U): Unit = carrier.updateAttachment(attachment)
    }
  }
  implicit class RichAttachable[T: Attachable](carrier: T) {
    def attachments: Attachments { type Pos = Position } = implicitly[Attachable[T]].attachments(carrier)
    def updateAttachment[U: ClassTag](attachment: U): Unit = implicitly[Attachable[T]].updateAttachment(carrier, attachment)
  }

  // NOTE: the mechanism of attachments is too low-level for our purposes
  // because it typically works with dedicated data structures that are declared and then shared between all potential users
  // since we are a compiler plugin, that's a big restriction, so we go for a stringly-typed approach
  // all metadata produced by the plugin is stored as key-value pairs in a java.util.HashMap[String, Any]
  // why Java's HashMap and not a native Scala Map? because prior to 2.11.2 attachments don't work  with subtyping and Map has a bunch of specific subclasses
  implicit class RichMetadataAttachable[T: Attachable](carrier: T) {
    def metadata: Metadata[T] = new Metadata(carrier)
    def appendMetadata(kvps: (String, Any)*): T = { kvps.foreach(kvp => carrier.metadata += kvp); carrier }
    def removeMetadata(keys: String*): T = { keys.foreach(key => carrier.metadata -= key); carrier }
    def hasMetadata(key: String): Boolean = metadata.get(key).isDefined
  }

  class Metadata[T: Attachable](carrier: T) {
    import scala.collection.JavaConversions._
    def toMap: Map[String, Any] = carrier.attachments.get[java.util.HashMap[String, Any]].map(_.toMap).getOrElse(Map[String, Any]())
    def toOption: Option[Map[String, Any]] = carrier.attachments.get[java.util.HashMap[String, Any]].map(_.toMap)
    def transform(f: Map[String, Any] => Map[String, Any]): Unit = carrier.updateAttachment(new java.util.HashMap[String, Any](mapAsJavaMap(f(toMap))))
    def contains(key: String): Boolean = toMap.contains(key)
    def apply(key: String): Any = toMap(key)
    def get(key: String): Option[Any] = toMap.get(key)
    def getOrElse[T: ClassTag](key: String, value: T): T = toMap.get(key).map(_.require[T]).getOrElse(value)
    def update(key: String, value: Any): Unit = transform(_ + (key -> value))
    def +=(kvp: (String, Any)): Unit = update(kvp._1, kvp._2)
    def ++=(other: Map[String, Any]): Unit = transform(_ ++ other)
    def ++=(other: Metadata[T]): Unit = transform(_ ++ other.toMap)
    def -=(key: String): Unit = transform(_ - key)
    def --=(other: List[String]): Unit = transform(_ -- other)
    def --=(other: Metadata[T]): Unit = transform(_ -- other.toMap.keys)
    override def toString = toMap.toString
  }
}