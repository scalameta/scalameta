package scala.meta
package internal
package ui

private[meta] object inferDialect {
  def apply(tree: Tree): Dialect = {
    // TODO: once we get Dotty-specific trees, update this logic
    scala.meta.dialects.Scala211
  }
}
