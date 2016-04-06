package scala.meta
package internal
package tql

import scala.language.experimental.macros
import org.scalameta.adt._
import scala.meta.tql._

object TraverserBuilder {
  def buildFromTopSymbolDelegate[T, A](f: Traverser[T]#Matcher[A], firsts: Any*): Traverser[T]#MatchResult[A] =
    macro TraverserBuilderMacros.buildFromTopSymbolDelegate[T, A]
}
