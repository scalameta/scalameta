package scala.meta
package internal

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.language.experimental.macros

package object dialects {
  def all: Seq[_] = macro Macros.all
}
