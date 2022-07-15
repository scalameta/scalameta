package scala.meta

package object tests {
  val Slow = new munit.Tag("Slow")
  val SkipWindows = new munit.Tag("SkipWindows")
}
