package example

class EtaExpansion/*<=example.EtaExpansion#*/ {
  Some/*=>scala.Some.*/(1).map/*=>scala.Option#map().*/(identity/*=>scala.Predef.identity().*/)
  List/*=>scala.collection.immutable.List.*/(1).foldLeft/*=>scala.collection.LinearSeqOptimized#foldLeft().*/("")(_/*<=local0*/ +/*=>java.lang.String#`+`().*/ _/*<=local1*/)

  def prop/*<=example.EtaExpansion#prop().*/ = ""
  def meth/*<=example.EtaExpansion#meth().*/() = ""

  prop/*=>example.EtaExpansion#prop().*/ _
  meth/*=>example.EtaExpansion#meth().*/ _
}
