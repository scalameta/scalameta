package org.scalameta.reflection

import scala.tools.nsc.Global

trait Metadata {
  val global: Global
  import global._
  import definitions._

  // NOTE: the mechanism of attachments is too low-level for our purposes
  // because it typically works with dedicated data structures that are declared and then shared between all potential users
  // since we are a compiler plugin, that's a big restriction, so we go for a stringly-typed approach
  // all tree-specific metadata produced by the plugin is stored as key-value pairs in a java.util.HashMap[String, Any]
  // why Java's HashMap and not a native Scala Map? because prior to 2.11.2 attachments don't work  with subtyping and Map has a bunch of specific subclasses
  implicit class RichMetadataTree[T <: Tree](tree: T) {
    def metadata: Metadata = new Metadata(tree)
    def appendMetadata(kvps: (String, Any)*): T = { kvps.foreach(kvp => tree.metadata += kvp); tree }
    def removeMetadata(keys: String*): T = { keys.foreach(key => tree.metadata -= key); tree }
    def scratchpad: List[Any] = tree.metadata.get("scratchpad").map(_.asInstanceOf[List[Any]]).getOrElse(Nil)
    def appendScratchpad(datum: Any): Tree = tree.appendMetadata("scratchpad" -> (tree.scratchpad :+ datum))
  }

  class Metadata(tree: Tree) {
    import scala.collection.JavaConversions._
    def toMap: Map[String, Any] = tree.attachments.get[java.util.HashMap[String, Any]].map(_.toMap).getOrElse(Map[String, Any]())
    def toOption: Option[Map[String, Any]] = tree.attachments.get[java.util.HashMap[String, Any]].map(_.toMap)
    def transform(f: Map[String, Any] => Map[String, Any]): Unit = tree.updateAttachment(new java.util.HashMap[String, Any](mapAsJavaMap(f(toMap))))
    def contains(key: String): Boolean = toMap.contains(key)
    def apply(key: String): Any = toMap(key)
    def get(key: String): Option[Any] = toMap.get(key)
    def getOrElse[T](key: String, value: T): T = toMap.get(key).map(_.asInstanceOf[T]).getOrElse(value)
    def update(key: String, value: Any): Unit = transform(_ + (key -> value))
    def +=(kvp: (String, Any)): Unit = update(kvp._1, kvp._2)
    def ++=(other: Map[String, Any]): Unit = transform(_ ++ other)
    def ++=(other: Metadata): Unit = transform(_ ++ other.toMap)
    def -=(key: String): Unit = transform(_ - key)
    def --=(other: List[String]): Unit = transform(_ -- other)
    def --=(other: Metadata): Unit = transform(_ -- other.toMap.keys)
    override def toString = toMap.toString
  }
}