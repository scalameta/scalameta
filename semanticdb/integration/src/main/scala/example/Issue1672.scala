// See https://github.com/scalameta/scalameta/issues/1672
package example

class Issue1672Annot(x: Int, y: String) extends scala.annotation.StaticAnnotation

@Issue1672Annot(y = "b", x = 2)
class Issue1672 {
  @Issue1672Annot(y = "b", x = 2)
  def namedDef = 1
  @Issue1672Annot(y = "b", x = 2)
  val namedVal = 1
  @Issue1672Annot(2, "b")
  def positionalDef = 1
  @Issue1672Annot({ val z = 2; z }, "b")
  def blockDef = 1
  def namedParam(@Issue1672Annot(y = "b", x = 2) p: Int) = p
  @Issue1672Annot(y = "b", x = 2)
  type NamedType = Int
  @Issue1672Annot(y = "b", x = 2)
  class NamedClass
}

object Issue1672 {
  @Issue1672Annot(y = "b", x = 2)
  def namedDef = 1
}
