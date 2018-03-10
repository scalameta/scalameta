package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala.{Names => n}

trait NameOps { self: DatabaseOps =>

  implicit class XtensionName(gname: g.Name) {
    def toSemantic: String = {
      if (gname == g.nme.ROOTPKG) {
        n.RootPackage
      } else if (gname == g.nme.EMPTY_PACKAGE_NAME) {
        n.EmptyPackage
      } else if (gname == g.nme.CONSTRUCTOR) {
        n.Constructor
      } else if (gname == g.tpnme.REFINE_CLASS_NAME) {
        // See https://github.com/scalameta/scalameta/pull/1109#discussion_r137194314
        // for a motivation why <refinement> symbols should have $anon as names.
        // This may be the wrong encoding of the symbol, but with the current
        // implementation it makes the use-site symbols of this refinement
        // decl match with the definition-site of the refinement decl.
        g.nme.ANON_CLASS_NAME.decoded
      } else {
        gname.decoded.stripSuffix(g.nme.LOCAL_SUFFIX_STRING)
      }
    }
  }
}
