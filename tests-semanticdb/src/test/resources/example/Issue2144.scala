package example
object Issue2144/*<=example.Issue2144.*/ {
  class Test/*<=example.Issue2144.Test#*/(a/*<=example.Issue2144.Test#a.*/: Boolean/*=>scala.Boolean#*/, b/*<=example.Issue2144.Test#b.*/: Int/*=>scala.Int#*/ = 1, c/*<=example.Issue2144.Test#c.*/: Int/*=>scala.Int#*/ = 2)
  val x/*<=example.Issue2144.x.*/ = new Test/*=>example.Issue2144.Test#*/(a/*=>example.Issue2144.Test#`<init>`().(a)*/ = true, c/*=>example.Issue2144.Test#`<init>`().(c)*/ = 1)
}
