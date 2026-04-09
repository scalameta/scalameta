package scala.meta
package internal
package tokens

import scala.{collection => sc}

object ScalaCompat {
  type IndexedSeqOptimized[+A] = sc.IndexedSeqOptimized[A, sc.immutable.IndexedSeq[A]]
}
