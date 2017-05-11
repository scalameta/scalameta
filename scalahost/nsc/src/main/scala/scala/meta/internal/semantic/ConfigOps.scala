package scala.meta.internal
package semantic

trait ConfigOps { self: DatabaseOps =>
  object config {
    override def toString = s"-Dscalameta.semanticdb=$style"
    private def style = sys.props.getOrElse("scalameta.semanticdb", "slim")
    def disabled = style == "none"
    def slim = style == "slim"
    def fat = style == "fat"
  }
}
