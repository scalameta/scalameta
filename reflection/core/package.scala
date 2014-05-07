package scala.reflect

import scala.language.experimental.{macros => prettyPlease}
import org.scalareflect.adt._
import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq

package object core {
  @hosted def root: Pkg.Root = delegate

  @hosted def syntaxProfile: SyntaxProfile = delegate
  final case class SyntaxProfile()

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

  // TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  // TODO: history vs positions (can trivia be inferred from positions only?)
  @root trait SourceContext
  object SourceContext {
    @leaf object None extends SourceContext
  }

  final case class ReflectionException(msg: String) extends Exception(msg)
}
