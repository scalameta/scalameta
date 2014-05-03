package scala.reflect

import scala.language.experimental.{macros => prettyPlease}
import org.scalareflect.adt._
import org.scalareflect.errors._

package object core {
  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )
  val unaryOps = Set("-", "+", "~", "!")
  implicit class Quasiquotes(ctx: StringContext) {
    protected trait api {
      def apply[T](args: T*): Tree = macro ???
      def unapply(scrutinee: Any): Any = macro ???
    }
    object q extends api
    object tq extends api
    object cq extends api
    object pq extends api
    object fq extends api
  }
  implicit class RichTypes(val parents: List[Type]) extends AnyVal {
    def linearization: List[Type] = ???
  }
  implicit class RichMods(val mods: List[Mod]) extends AnyVal {
    def has[T <: Mod](implicit tag: ClassTag[T]): Boolean =
      mods.exists { _.getClass == tag.runtimeClass }
  }
  def lub(tpes: Type*): Type = ???
  def glb(tpes: Type*): Type = ???
  @root trait SemanticError extends Exception
}

package core {
  package object errors {
    implicit val throwExceptions = handlers.throwExceptions
    implicit val returnTries = handlers.returnTries
  }
}
