package example

trait SymbolTest/*<=example.SymbolTest#*/ {
  def shouldBe/*<=example.SymbolTest#shouldBe().*/(right/*<=example.SymbolTest#shouldBe().(right)*/: Any/*=>scala.Any#*/): Unit/*=>scala.Unit#*/
  def arg/*<=example.SymbolTest#arg().*/ = 1
  this shouldBe/*=>example.SymbolTest#shouldBe().*/ (arg/*=>example.SymbolTest#arg().*/) // see: https://github.com/scalacenter/scalafix/issues/1594
}
