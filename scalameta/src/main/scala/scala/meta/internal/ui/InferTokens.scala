package scala.meta
package internal
package ui

import scala.meta.dialects.Scala211

private[meta] object inferTokens {
  def apply(tree: Tree): Tokens = { // TODO, dummy for now
  	val code = tree.show[Code]
  	(tree match {
  		case _: Source => code.parse[Source]
  		case _: Stat => code.parse[Stat]
  	}).origin.tokens
  }
}
