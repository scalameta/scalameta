package scala.reflect

import scala.language.experimental.{macros => pleaseEnableMacros}

package object core {
  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )
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
  implicit class RichParents(val parents: List[Aux.Parent]) {
    // TODO: not sure whether this is the best approach
    def linearization: List[Aux.Parent] = ???
  }
  def lub(typs: Type*): Type = ???
  def glb(typs: Type*): Type = ???
}