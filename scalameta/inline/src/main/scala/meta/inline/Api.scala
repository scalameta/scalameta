package scala.meta
package inline

private[meta] trait Api {
  // Enables `meta` blocks, the foundation for new-style ("inline") macros.
  // This is a magic method whose argument is typechecked using special rules.
  // See https://github.com/scalameta/paradise for more information.
  def apply(body: Any): Any = ???
}

private[meta] trait Aliases {
}
