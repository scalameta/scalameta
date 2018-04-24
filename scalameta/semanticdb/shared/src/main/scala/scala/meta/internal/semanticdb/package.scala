package scala.meta.internal

import scala.language.implicitConversions

package object semanticdb {
  object schema {
    val Document = scala.meta.internal.semanticdb3.TextDocument
    type Document = scala.meta.internal.semanticdb3.TextDocument
    // TODO
  }
}
