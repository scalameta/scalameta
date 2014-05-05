package scala.reflect
import scala.language.experimental.{macros => prettyPlease}
import scala.reflect.quasiquotes.quasiquote

package object core {
  // three primary quasiquotes
  @quasiquote('q)      implicit class TermQuote(ctx: StringContext)
  @quasiquote('t)      implicit class TypeQuote(ctx: StringContext)
  @quasiquote('p)      implicit class PatternQuote(ctx: StringContext)

  // auxilary quasiquotes
  @quasiquote('templ)  implicit class TemplateQuote(ctx: StringContext)
  @quasiquote('param)  implicit class ParamQuote(ctx: StringContext)
  @quasiquote('tparam) implicit class TypeParamQuote(ctx: StringContext)
  @quasiquote('parent) implicit class ParentQuote(ctx: StringContext)
  @quasiquote('self)   implicit class SelfQuote(ctx: StringContext)
  @quasiquote('enum)   implicit class EnumQuote(ctx: StringContext)
  @quasiquote('mod)    implicit class ModQuote(ctx: StringContext)
  @quasiquote('arg)    implicit class ArgQuote(ctx: StringContext)
  @quasiquote('cas)    implicit class CaseQuote(ctx: StringContext)

  implicit class RichTypes(val parents: List[Type]) extends AnyVal {
    def linearization: List[Type] = ???
  }

  def lub(tpes: Type*): Type = ???
  def glb(tpes: Type*): Type = ???
}
