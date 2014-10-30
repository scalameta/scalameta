package scala

import scala.language.implicitConversions
import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.convert._

package object meta {
  @quasiquote('q)     implicit class TermQuote(ctx: StringContext)
  @quasiquote('t)     implicit class TypeQuote(ctx: StringContext)
  @quasiquote('p)     implicit class PatternQuote(ctx: StringContext)
  @quasiquote('param) implicit class ParamQuote(ctx: StringContext)
  @quasiquote('arg)   implicit class ArgQuote(ctx: StringContext)
  @quasiquote('enum)  implicit class EnumQuote(ctx: StringContext)
  @quasiquote('mod)   implicit class ModQuote(ctx: StringContext)
  @quasiquote('templ) implicit class TemplateQuote(ctx: StringContext)
  @quasiquote('sel)   implicit class SelectorQuote(ctx: StringContext)
  @quasiquote('cas)   implicit class CaseQuote(ctx: StringContext)

  final case class MetaException(msg: String) extends Exception(msg)
}
