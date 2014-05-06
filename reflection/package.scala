package scala.reflect

import scala.language.experimental.{macros => prettyPlease}
import org.scalareflect.adt._
import org.scalareflect.errors._

package object core {
  def root: Pkg.Root = ???

  def languageProfile: LanguageProfile = ???
  final case class LanguageProfile(
    dynamics: Boolean,
    postfixOps: Boolean,
    reflectiveCalls: Boolean,
    implicitConversions: Boolean,
    higherKinds: Boolean,
    existentials: Boolean,
    macros: Boolean
    // TODO: do we support Scala 2.10 and 2.11?
  )

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

  implicit class RichTemplates(val parents: List[Member.Template]) extends AnyVal {
    def linearization: List[Member.Template] = ???
  }
  implicit class RichTypes(val parents: List[Type]) extends AnyVal {
    def linearization: List[Type] = ???
  }
  def lub(tpes: Type*): Type = ???
  def glb(tpes: Type*): Type = ???

  object c {
    def macroApplication: Tree = ???
    // TODO: design a way to specify positions here
    def warning(msg: String): Unit = ???
    def error(msg: String): Unit = ???
    def abort(msg: String): Nothing = ???
    final case class Resource(url: String) {
      def read(implicit codec: scala.io.Codec): String = ???
    }
    class Resources(urls: List[String]) extends Iterable[Resource] {
      def iterator: Iterator[Resource] = ???
      def apply(url: String)(implicit codec: scala.io.Codec): String = ???
    }
    def resources: Resources = ???
  }

  // TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  // TODO: history vs positions (can trivia be inferred from positions only?)
  @root trait SourceContext {
  }

  final case class ReflectionException(msg: String) extends Exception(msg)
}

package core {
  package object errors {
    implicit val throwExceptions = handlers.throwExceptions
    implicit val returnTries = handlers.returnTries
  }
}
