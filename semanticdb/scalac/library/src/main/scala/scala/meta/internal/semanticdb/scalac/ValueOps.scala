package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Names => n}

trait ValueOps { self: SemanticdbOps =>

  implicit class XtensionName(gname: g.Name) {
    def toSemantic: String = {
      if (gname == g.nme.ROOTPKG) n.RootPackage.value
      else if (gname == g.nme.EMPTY_PACKAGE_NAME) n.EmptyPackage.value
      else if (gname == g.nme.CONSTRUCTOR) n.Constructor.value
      else if (gname.startsWith("_$")) "_"
      else gname.decoded.stripSuffix(g.nme.LOCAL_SUFFIX_STRING)
    }
  }
}
