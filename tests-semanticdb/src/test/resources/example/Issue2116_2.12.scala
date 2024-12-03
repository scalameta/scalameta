// See https://github.com/scalameta/scalameta/issues/2116
package example

import scala.concurrent.ExecutionContext/*=>scala.concurrent.ExecutionContext.*//*=>scala.concurrent.ExecutionContext#*/

abstract class Issue2116/*<=example.Issue2116#*/ {

  def check/*<=example.Issue2116#check().*/(
      includeDocs/*<=example.Issue2116#check().(includeDocs)*/: Boolean/*=>scala.Boolean#*/ = false,
      includeCommitCharacter/*<=example.Issue2116#check().(includeCommitCharacter)*/: Boolean/*=>scala.Boolean#*/ = false
  )(implicit loc/*<=example.Issue2116#check().(loc)*/: ExecutionContext/*=>scala.concurrent.ExecutionContext#*/): Unit/*=>scala.Unit#*/ = {}
}

class Issue2116_2/*<=example.Issue2116_2#*/ extends Issue2116/*=>example.Issue2116#*/ {

  implicit val ec/*<=example.Issue2116_2#ec.*/ = scala.concurrent.ExecutionContext/*=>scala.concurrent.ExecutionContext.*/.global/*=>scala.concurrent.ExecutionContext.global().*/

  check/*=>example.Issue2116#check().*/(
    includeCommitCharacter/*=>example.Issue2116#check().(includeCommitCharacter)*/ = true
  )

}
