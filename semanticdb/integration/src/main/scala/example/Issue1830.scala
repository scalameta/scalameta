// See https://github.com/scalameta/scalameta/issues/1830
package example

import scala.language.higherKinds

trait Issue1830 {
  type T[_, _]
  def m[F[_, _]]: Unit
}

class Issue1830Single[_] {
  def m[_]: Unit = ()
  type A[_] = Int
  def hk[G[_]]: Unit = ()
}
