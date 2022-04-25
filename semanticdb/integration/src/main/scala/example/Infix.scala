package example

trait SymbolTest {
  def shouldBe(right: Any): Unit
  def arg = 1
  this shouldBe (arg) // see: https://github.com/scalacenter/scalafix/issues/1594
}
