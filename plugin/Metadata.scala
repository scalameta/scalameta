package scala.meta
package internal.hosts.scalacompiler

import scala.meta.internal.hosts.scalacompiler.{Plugin => PalladiumPlugin}

trait Metadata { self: PalladiumPlugin =>
  import global._
  import definitions._

  // NOTE: the mechanism of attachments is too low-level for our purposes
  // because it typically works with dedicated data structures that are declared and then shared between all potential users
  // since we are a compiler plugin, that's a big restriction, so we go for a stringly-typed approach
  // all tree-specific metadata produced by the plugin is stored as key-value pairs in a java.util.HashMap[String, Any]
  // why Java's HashMap and not a native Scala Map? because prior to 2.11.2 attachments don't work  with subtyping and Map has a bunch of specific subclasses
  implicit class RichTree(tree: Tree) {
    def metadata: Metadata = new Metadata(tree)
  }

  class Metadata(tree: Tree) {
    import scala.collection.JavaConversions._
    def underlying: Map[String, Any] = tree.attachments.get[java.util.HashMap[String, Any]].map(_.toMap).getOrElse(Map[String, Any]())
    def transform(f: Map[String, Any] => Map[String, Any]): Unit = tree.updateAttachment(new java.util.HashMap[String, Any](mapAsJavaMap(f(underlying))))
    def apply(key: String): Any = underlying(key)
    def get(key: String): Any = underlying.get(key)
    def update(key: String, value: Any): Unit = transform(_ + (key -> value))
    def +=(key: String, value: Any): Unit = update(key, value)
    def +=(kvp: (String, Any)): Unit = update(kvp._1, kvp._2)
    def -=(key: String): Unit = transform(_ - key)
    def ++=(other: Map[String, Any]): Unit = transform(_ ++ other)
    def ++=(other: Metadata): Unit = transform(_ ++ other.underlying)
  }
}