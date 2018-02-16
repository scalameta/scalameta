package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}

trait NameOps { self: DatabaseOps =>

  implicit class XtensionName(gname: g.Name) {
    def toSemantic: String = {
      if (gname == g.tpnme.REFINE_CLASS_NAME) {
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
    def toEncodedSemantic: String = {
      if (gname.isEmpty) {
        sys.error(s"unsupported name")
      } else {
        val name = gname.toSemantic
        val (start, parts) = (name.head, name.tail)
        val isStartOk = Character.isJavaIdentifierStart(start)
        val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
        if (isStartOk && isPartsOk) name
        else "`" + name + "`"
      }
    }
  }
}
