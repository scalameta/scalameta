package example

object Issue2882/*<=example.Issue2882.*/ {

  case class FooFailure/*<=example.Issue2882.FooFailure#*/(msg/*<=example.Issue2882.FooFailure#msg.*/: String/*=>scala.Predef.String#*/)
    extends Exception/*=>scala.package.Exception#*//*=>java.lang.Exception#`<init>`(+1).*/(s/*=>scala.StringContext#s().*/"Error: $msg/*=>example.Issue2882.FooFailure#*/")

}
