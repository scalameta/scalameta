package example

class EtaExpansion/*<=example.EtaExpansion#*/ {
  Some/*=>scala.Some.*/(1).map/*=>scala.Option#map().*/(identity/*=>scala.Predef.identity().*/)
  List/*=>scala.package.List.*/(1).foldLeft/*=>scala.collection.LinearSeqOps#foldLeft().*/("")(_ +/*=>java.lang.String#`+`().*/ _)

  def prop/*<=example.EtaExpansion#prop().*/ = ""
  def meth/*<=example.EtaExpansion#meth().*/() = ""

  prop/*=>example.EtaExpansion#prop().*/ _
  meth/*=>example.EtaExpansion#meth().*/ _
}
