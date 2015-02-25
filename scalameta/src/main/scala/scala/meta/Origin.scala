package scala.meta

import org.scalameta.adt._

@root trait Origin
object Origin {
  @leaf object None extends Origin
  @leaf class Parsed(tokens: Vector[Token]) extends Origin
  @leaf class Transformed(tree: Tree) extends Origin // TODO: also include information about the transformer
}