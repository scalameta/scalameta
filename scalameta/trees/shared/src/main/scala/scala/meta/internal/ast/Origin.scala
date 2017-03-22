package scala.meta
package internal
package ast

import org.scalameta.adt
import scala.meta.common._
import scala.meta.inputs._
import scala.meta.internal.tokens._

@adt.root trait Origin extends Optional
object Origin {
  @adt.none object None extends Origin
  @adt.leaf class Parsed(input: Input, dialect: Dialect, pos: TokenStreamPosition) extends Origin
}