package org.scalameta
package dialects

import scala.reflect.macros.whitebox.Context

class CurrentDialect(val c: Context) {
  import c.universe._
  import definitions._

  def impl: c.Tree = {
    // TODO: We'll have to expand this in the future,
    // but for now the only other supported dialect is Dotty,
    // and we don't yet have a dottyhost, so yolo.
    q"_root_.scala.meta.dialects.Scala211"
  }
}
