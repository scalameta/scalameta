// See https://github.com/scalameta/scalameta/issues/1672
package example

class Issue1672Annot/*<=example.Issue1672Annot#*/(x/*<=example.Issue1672Annot#`<init>`().(x)*//*<=example.Issue1672Annot#x.*/: Int/*=>scala.Int#*/, y/*<=example.Issue1672Annot#`<init>`().(y)*//*<=example.Issue1672Annot#y.*/: String/*=>scala.Predef.String#*/) extends scala.annotation.StaticAnnotation/*=>scala.annotation.StaticAnnotation#*//*=>scala.annotation.Annotation#`<init>`().*/

@Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2)
class Issue1672/*<=example.Issue1672#*/ {
  @Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2)
  def namedDef/*<=example.Issue1672#namedDef().*/ = 1
  @Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2)
  val namedVal/*<=example.Issue1672#namedVal.*/ = 1
  @Issue1672Annot/*=>example.Issue1672Annot#*/(2, "b")
  def positionalDef/*<=example.Issue1672#positionalDef().*/ = 1
  @Issue1672Annot/*=>example.Issue1672Annot#*/({ val z/*<=local0*/ = 2; z/*=>local0*/ }, "b")
  def blockDef/*<=example.Issue1672#blockDef().*/ = 1
  def namedParam/*<=example.Issue1672#namedParam().*/(@Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2) p/*<=example.Issue1672#namedParam().(p)*/: Int/*=>scala.Int#*/) = p/*=>example.Issue1672#namedParam().(p)*/
  @Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2)
  type NamedType/*<=example.Issue1672#NamedType#*/ = Int/*=>scala.Int#*/
  @Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2)
  class NamedClass/*<=example.Issue1672#NamedClass#*/
}

object Issue1672/*<=example.Issue1672.*/ {
  @Issue1672Annot/*=>example.Issue1672Annot#*/(y/*=>example.Issue1672Annot#`<init>`().(y)*/ = "b", x/*=>example.Issue1672Annot#`<init>`().(x)*/ = 2)
  def namedDef/*<=example.Issue1672.namedDef().*/ = 1
}
