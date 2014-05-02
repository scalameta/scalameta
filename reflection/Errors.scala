package scala.reflect
package core

import org.scalareflect.adt._
import org.scalareflect.errors._

@root trait SemanticError extends Exception

package object errors {
  implicit val throwExceptions = handlers.throwExceptions
  implicit val returnTries = handlers.returnTries
}