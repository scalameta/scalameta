package scala.meta
package internal
package ast

import org.scalameta.adt
import scala.meta.common._
import scala.meta.inputs._

@adt.root trait Origin extends Optional
object Origin {
  @adt.none object None extends Origin
  @adt.leaf class Parsed(input: Input, dialect: Dialect, pos: Position) extends Origin
}