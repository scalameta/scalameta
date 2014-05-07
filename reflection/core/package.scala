package scala.reflect

import scala.language.experimental.{macros => prettyPlease}
import org.scalareflect.adt._
import org.scalareflect.errors._
import scala.reflect.core.errors.{wrapHosted, wrapMacrohosted}
import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq

package object core {
  @hosted def root: Pkg.Root = delegate

  @hosted def languageProfile: LanguageProfile = delegate
  final case class LanguageProfile(
    dynamics: Boolean,
    postfixOps: Boolean,
    reflectiveCalls: Boolean,
    implicitConversions: Boolean,
    higherKinds: Boolean,
    existentials: Boolean,
    macros: Boolean
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

  @hosted private[core] def supertypesToMembers(tpes: Seq[Type]): Seq[Member.Template] = {
    def extractTemplate(ref: Type.Ref) = {
      for {
        defn <- ref.defn
        result <- defn match {
          case t: Member.Template => succeed(t)
          case d => fail(ReflectionException(s"unexpected ref $ref to $d returned from supertypes"))
        }
      } yield result
    }
    succeed(tpes) mmap {
      case ref: Type.Ref => extractTemplate(ref)
      case Type.Apply(ref: Type.Ref, _) => extractTemplate(ref)
      case tpe => fail(ReflectionException(s"unexpected type $tpe returned from supertypes"))
    }
  }
  implicit class RichTemplates(val parents: Seq[Member.Template]) extends AnyVal {
    @hosted def linearization: Seq[Member.Template] = {
      val linearization = parents.map(p => ??? : Type).linearization // TODO: t"${p.ref}"
      linearization.flatMap(tpes => supertypesToMembers(tpes))
    }
  }
  implicit class RichTypes(val parents: Seq[Type]) extends AnyVal {
    @hosted def linearization: Seq[Type] = wrapHosted(_.linearization(parents))
  }
  @hosted def lub(tpes: Type*): Type = delegate
  @hosted def glb(tpes: Type*): Type = delegate

  object c {
    @hosted(macroApi = true) def macroApplication: Tree = delegate
    // TODO: design a way to specify positions here
    @hosted(macroApi = true) def warning(msg: String): Unit = delegate
    @hosted(macroApi = true) def error(msg: String): Unit = delegate
    @hosted(macroApi = true) def abort(msg: String): Nothing = delegate
    final case class Resource(url: String)(implicit mc: MacroContext) {
      @mayFail def read(implicit codec: scala.io.Codec): String = wrapMacrohosted(_.readResource(url, codec))
    }
    class Resources(urls: Seq[String])(implicit mc: MacroContext) extends Iterable[Resource] {
      def iterator: Iterator[Resource] = urls.map(url => new Resource(url)).iterator
      @mayFail def apply(url: String)(implicit codec: scala.io.Codec): String = new Resource(url).read
    }
    @hosted(macroApi = true) def resources: Resources = wrapMacrohosted(mc => new Resources(mc.listResources))
  }

  // TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  // TODO: history vs positions (can trivia be inferred from positions only?)
  @root trait SourceContext

  final case class ReflectionException(msg: String) extends Exception(msg)
}

package core {
  package object errors {
    implicit val throwExceptions = handlers.throwExceptions
    implicit val returnTries = handlers.returnTries

    def succeed[S](x: S)(implicit eh: ErrorHandler): eh.Success[S] = eh.succeed(x)
    def fail[E <: Exception](x: E)(implicit eh: ErrorHandler): eh.Failure[E] = eh.fail(x)

    def wrapHosted = new Wrap[HostContext]
    def wrapMacrohosted = new Wrap[MacroContext]
    class Wrap[C <: HostContext] {
      def apply[T](f: C => T)(implicit c: C, eh: ErrorHandler) = {
        try eh.succeed(f(c))
        catch {
          case ex: ReflectionException => eh.fail(ex)
          case ex: Exception => eh.fail(ReflectionException(ex.toString))
        }
      }
    }
  }
}
