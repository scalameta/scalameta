package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala.{Names => n}

trait NameOps { self: SemanticdbOps =>

  implicit class XtensionName(gname: g.Name) {
    def toSemantic: String = {
      if (gname == g.nme.ROOTPKG) {
        n.RootPackage
      } else if (gname == g.nme.EMPTY_PACKAGE_NAME) {
        n.EmptyPackage
      } else if (gname == g.nme.CONSTRUCTOR) {
        n.Constructor
      } else if (gname.startsWith("_$")) {
        n.Anonymous
      } else {
        gname.decoded.stripSuffix(g.nme.LOCAL_SUFFIX_STRING)
      }
    }
  }
}
