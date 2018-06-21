package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.scalacp._
import scala.meta.internal.{semanticdb => s}

trait AnnotationOps { self: SemanticdbOps =>

  implicit class XtensionAnnotationInfo(gann: g.AnnotationInfo) {
    def toSemantic: s.Annotation = {
      s.Annotation(gann.atp.toSemanticTpe)
    }
  }
}
