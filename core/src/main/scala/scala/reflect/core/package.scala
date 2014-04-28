package scala.reflect
import scala.language.experimental.{macros => prettyPlease}

package object core {
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
}
