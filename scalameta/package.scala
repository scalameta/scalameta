package scala

import scala.language.implicitConversions
import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.convert._

package object meta {
  @quasiquote[Stmt]('q)             implicit class TermQuote(ctx: StringContext)
  @quasiquote[Param.Type]('t)       implicit class TypeQuote(ctx: StringContext)
  @quasiquote[Pat]('p)              implicit class PatternQuote(ctx: StringContext)

  @quasiquote[Param]('param)        implicit class ParamQuote(ctx: StringContext)
  @quasiquote[TypeParam]('tparam)   implicit class TypeParamQuote(ctx: StringContext)
  @quasiquote[Arg]('arg)            implicit class ArgQuote(ctx: StringContext)
  @quasiquote[Enum]('enum)          implicit class EnumQuote(ctx: StringContext)
  @quasiquote[Mod]('mod)            implicit class ModQuote(ctx: StringContext)
  @quasiquote[Aux.Case]('cas)       implicit class CaseQuote(ctx: StringContext)
  @quasiquote[Aux.Parent]('parent)  implicit class ParentQuote(ctx: StringContext)
  @quasiquote[Aux.Template]('templ) implicit class TemplateQuote(ctx: StringContext)
  @quasiquote[Aux.Self]('self)      implicit class SelfQuote(ctx: StringContext)

  @root trait Source { def content: Array[Char] }
  object Source {
    @leaf object None extends Source { def content = new Array[Char](0) }
    @leaf class String(s: scala.Predef.String) extends Source {
      lazy val content = s.toArray
    }
    @leaf class File(f: java.io.File) extends Source {
      lazy val content = scala.io.Source.fromFile(f).mkString.toArray
    }
    object File {
      def apply(path: Predef.String): Source.File = Source.File(new java.io.File(path))
    }
    implicit val stringToSource: Convert[scala.Predef.String, Source] = Convert.apply(Source.String(_))
    implicit val fileToSource: Convert[java.io.File, Source] = Convert.apply(Source.File(_))
  }
  @root trait Origin { def src: Source }
  object Origin {
    @leaf object None extends Origin { def src = meta.Source.None }
    @leaf class Source(src: meta.Source) extends Origin // TODO: positions
    @leaf class Transform(proto: Tree, origin: Origin) extends Origin { def src = origin.src }
    implicit def defaultOriginIsNone: Origin = None
  }

  final case class MetaException(msg: String) extends Exception(msg)
}
