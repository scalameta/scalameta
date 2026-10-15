// See https://github.com/scalameta/scalameta/issues/1806
package example

class Issue1806/*<=example.Issue1806#*/(x/*<=example.Issue1806#x.*//*<=example.Issue1806#`<init>`().(x)*/: Issue1806/*=>example.Issue1806#*/ { type T/*<=local0*/ = Issue1806/*=>example.Issue1806#*/ with Issue1806/*=>example.Issue1806#*/ }) {
  val inParent/*<=example.Issue1806#inParent.*/: (Issue1806/*=>example.Issue1806#*/ { type U/*<=local1*/ }) { type T/*<=local2*/ } = ???/*=>scala.Predef.`???`().*/
  val inDecl/*<=example.Issue1806#inDecl.*/: Issue1806/*=>example.Issue1806#*/ { def m/*<=local3*/: Issue1806/*=>example.Issue1806#*/ { def n/*<=local4*/: Int/*=>scala.Int#*/ } } = ???/*=>scala.Predef.`???`().*/
  val inArg/*<=example.Issue1806#inArg.*/: List/*=>scala.package.List#*/[Issue1806/*=>example.Issue1806#*/ { type T/*<=local5*/ = Issue1806/*=>example.Issue1806#*/ with Issue1806/*=>example.Issue1806#*/ }] = ???/*=>scala.Predef.`???`().*/
  val flat/*<=example.Issue1806#flat.*/: Issue1806/*=>example.Issue1806#*/ { type T/*<=local6*/ } = ???/*=>scala.Predef.`???`().*/
  type Alias/*<=example.Issue1806#Alias#*/ = Issue1806/*=>example.Issue1806#*/ { type T/*<=local7*/ = Issue1806/*=>example.Issue1806#*/ with Issue1806/*=>example.Issue1806#*/ }
}
