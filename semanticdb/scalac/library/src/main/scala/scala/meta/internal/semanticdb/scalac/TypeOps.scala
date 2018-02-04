package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}

trait TypeOps { self: DatabaseOps =>
  implicit class XtensionGTypeSType(tpe: g.Type) {
    def toSemantic: (s.Type, List[g.Symbol]) = {
      ???
    }
  }
}
