package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}

trait AnnotationOps { self: Scalacp =>
  implicit class XtensionAnnotation(ann: Int) {
    def toSemantic: s.AnnotationTree = {
      // FIXME: https://github.com/scalameta/scalameta/issues/1292
      s.AnnotationTree()
    }
  }
}
