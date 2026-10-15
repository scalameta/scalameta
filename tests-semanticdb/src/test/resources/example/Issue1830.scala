// See https://github.com/scalameta/scalameta/issues/1830
package example

import scala.language/*=>scala.language.*/.higherKinds/*=>scala.language.higherKinds.*/

trait Issue1830/*<=example.Issue1830#*/ {
  type T/*<=example.Issue1830#T#*/[_, _]
  def m/*<=example.Issue1830#m().*/[F/*<=example.Issue1830#m().[F]*/[_, _]]: Unit/*=>scala.Unit#*/
}

class Issue1830Single/*<=example.Issue1830Single#*/[_] {
  def m/*<=example.Issue1830Single#m().*/[_]: Unit/*=>scala.Unit#*/ = ()
  type A/*<=example.Issue1830Single#A#*/[_] = Int/*=>scala.Int#*/
  def hk/*<=example.Issue1830Single#hk().*/[G/*<=example.Issue1830Single#hk().[G]*/[_]]: Unit/*=>scala.Unit#*/ = ()
}
