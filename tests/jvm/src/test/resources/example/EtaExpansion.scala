package example

class EtaExpansion/*<=example.EtaExpansion#*/ {
  Some/*=>scala.Some.*/(1).map/*=>scala.Option#map().*/(identity/*=>scala.Predef.identity().*/)
  List/*=>scala.collection.immutable.List.*/(1).foldLeft/*=>scala.collection.LinearSeqOptimized#foldLeft().*/("")(_ +/*=>java.lang.String#`+`().*/ _)

  def prop/*<=example.EtaExpansion#prop().*/ = ""
  def meth/*<=example.EtaExpansion#meth().*/() = ""

  prop/*=>local1*/ _
  meth/*=>local2*/ _
}
