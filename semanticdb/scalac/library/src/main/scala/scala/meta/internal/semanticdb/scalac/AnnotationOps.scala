package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}

trait AnnotationOps { self: DatabaseOps =>

  implicit class XtensionAnnotationInfo(gann: g.AnnotationInfo) {
    def toSemantic: (s.Annotation, List[g.Symbol]) = {
      val (stpe, todo) = gann.atp.toSemantic
      (s.Annotation(stpe), todo)
    }
  }
}
