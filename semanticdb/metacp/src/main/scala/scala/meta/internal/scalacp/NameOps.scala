package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala.{Names => n}
import scala.reflect.NameTransformer

trait NameOps { self: Scalacp =>
  implicit class XtensionName(name: String) {
    def toSemantic: String = {
      val i = name.lastIndexOf("$$")
      if (i > 0) name.substring(i + 2).toSemantic
      else if (name == "<root>") n.RootPackage
      else if (name == "<empty>") n.EmptyPackage
      else if (name == "<init>") n.Constructor
      else if (name.startsWith("_$")) n.Anonymous
      else NameTransformer.decode(name).stripSuffix(" ")
    }
  }
}
