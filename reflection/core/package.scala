package scala.reflect

import scala.language.experimental.{macros => prettyPlease}
import org.scalareflect.adt._
import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq

package object core {
  @hosted def syntaxProfile: SyntaxProfile = delegate
  final case class SyntaxProfile()

  @quasiquote('q)      implicit class TermQuote(ctx: StringContext)
  @quasiquote('t)      implicit class TypeQuote(ctx: StringContext)
  @quasiquote('p)      implicit class PatternQuote(ctx: StringContext)
  @quasiquote('templ)  implicit class TemplateQuote(ctx: StringContext)
  @quasiquote('param)  implicit class ParamQuote(ctx: StringContext)
  @quasiquote('tparam) implicit class TypeParamQuote(ctx: StringContext)
  @quasiquote('parent) implicit class ParentQuote(ctx: StringContext)
  @quasiquote('self)   implicit class SelfQuote(ctx: StringContext)
  @quasiquote('enum)   implicit class EnumQuote(ctx: StringContext)
  @quasiquote('mod)    implicit class ModQuote(ctx: StringContext)
  @quasiquote('arg)    implicit class ArgQuote(ctx: StringContext)
  @quasiquote('cas)    implicit class CaseQuote(ctx: StringContext)

  // TODO: trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  // TODO: history vs positions (can trivia be inferred from positions only?)
  // TODO: what should be there in transform? what should be the signature of mapXXX and withXXX?
  // TODO: also there's the question of what origin should SemanticMemberOps.ref produce for empty This
  @root trait Source { def content: Array[Char] }
  object Source {
    @leaf object None extends Source { def content = new Array[Char](0) }
    @leaf class String(s: scala.Predef.String) extends Source {
      lazy val content = s.toArray
    }
    @leaf class File(f: java.io.File) extends Source {
      lazy val content = scala.io.Source.fromFile(f).mkString.toArray
    }
    object File { def apply(path: Predef.String): Source.File = Source.File(new java.io.File(path)) }
  }
  @root trait Origin { def src: Source }
  object Origin {
    @leaf object None extends Origin { def src = core.Source.None }
    @leaf class Source(src: core.Source) extends Origin
    @leaf class Transform(proto: Tree, origin: Origin) extends Origin { def src = origin.src }
    implicit def defaultOriginIsNone: Origin = None
  }

  final case class ReflectionException(msg: String) extends Exception(msg)
}
