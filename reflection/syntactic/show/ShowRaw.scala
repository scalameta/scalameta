package scala.reflect
package syntactic.show

import core._
import org.scalareflect.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq

object ShowRaw {
  implicit def showTree[T <: Tree]: Show[T] = Show(x => {
    s(x.productPrefix, "(", r(x.productIterator.map(v => if (v != null) v.toString else "null").toList, ", "), ")")
  })
}
