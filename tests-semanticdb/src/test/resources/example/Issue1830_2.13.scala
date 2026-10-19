// See https://github.com/scalameta/scalameta/issues/1830
package example

import scala.language/*=>scala.language.*/.higherKinds/*=>scala.language.higherKinds.*/

trait Issue1830/*<=example.Issue1830#*/ {
  type T/*<=example.Issue1830#T#*/[_/*<=example.Issue1830#T#[_$$1]*/, _/*<=example.Issue1830#T#[_$$2]*/]
  def m/*<=example.Issue1830#m().*/[F/*<=example.Issue1830#m().[F]*/[_/*<=example.Issue1830#m().[F][_$$3]*/, _/*<=example.Issue1830#m().[F][_$$4]*/]]: Unit/*=>scala.Unit#*/
}

class Issue1830Single/*<=example.Issue1830Single#*/[_/*<=example.Issue1830Single#[_$$5]*/] {
  def m/*<=example.Issue1830Single#m().*/[_/*<=example.Issue1830Single#m().[_$$6]*/]: Unit/*=>scala.Unit#*/ = ()
  type A/*<=example.Issue1830Single#A#*/[_/*<=example.Issue1830Single#A#[_$$7]*/] = Int/*=>scala.Int#*/
  def hk/*<=example.Issue1830Single#hk().*/[G/*<=example.Issue1830Single#hk().[G]*/[_/*<=example.Issue1830Single#hk().[G][_$$8]*/]]: Unit/*=>scala.Unit#*/ = ()
}
