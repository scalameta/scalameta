package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Names => n}
import scala.reflect.NameTransformer

trait ValueOps { self: Scalacp =>
  implicit class XtensionValue(value: String) {
    def toSemantic: String = {
      val i = value.lastIndexOf("$$")
      if (i > 0) value.substring(i + 2).toSemantic
      else if (value == "<root>") n.RootPackage.value
      else if (value == "<empty>") n.EmptyPackage.value
      else if (value == "<init>") n.Constructor.value
      else if (value.startsWith("_$")) "_"
      else NameTransformer.decode(value).stripSuffix(" ")
    }
  }
}
