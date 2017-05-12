package scala.meta.internal
package semantic

import scala.meta.io._

trait ConfigOps { self: DatabaseOps =>
  object config {
    override def toString = s"-Dscalameta.semanticdb=$style -Dscalameta.sourcepath=$sourcepath"

    private def style = sys.props.getOrElse("scalameta.semanticdb", "slim")
    def disabled: Boolean = style == "none"
    def slim: Boolean = style == "slim"
    def fat: Boolean = style == "fat"

    def sourcepath: Sourcepath =
      Sourcepath(sys.props.getOrElse("scalameta.sourcepath", sys.props("user.dir")))
  }
}
