// See https://github.com/scalameta/scalameta/issues/2116
package example

import scala.concurrent.ExecutionContext

abstract class Issue2116 {

  def check(
      includeDocs: Boolean = false,
      includeCommitCharacter: Boolean = false
  )(implicit loc: ExecutionContext): Unit = {}
}

class Issue2116_2 extends Issue2116 {

  implicit val ec = scala.concurrent.ExecutionContext.global

  check(
    includeCommitCharacter = true
  )

}
