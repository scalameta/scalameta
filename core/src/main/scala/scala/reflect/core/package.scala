package scala.reflect
import scala.language.experimental.{macros => prettyPlease}

package object core {
  implicit class Quasiquotes(ctx: StringContext) {
    protected trait api {
      def apply[T](args: T*): Tree = macro ???
      def unapply(scrutinee: Any): Any = macro ???
    }
    object q extends api
    object t extends api
    object p extends api
    // consider also adding templ, param, tparam, parent, self, case, enum, mod, arg interpolators
  }
  implicit class RichTypes(val parents: List[Type]) extends AnyVal {
    def linearization: List[Type] = ???
  }

  def lub(tpes: Type*): Type = ???
  def glb(tpes: Type*): Type = ???
}
