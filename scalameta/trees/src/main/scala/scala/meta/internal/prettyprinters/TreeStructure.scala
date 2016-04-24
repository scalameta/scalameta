package scala.meta
package internal
package prettyprinters

import scala.meta.classifiers._
import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.ast.Quasi

object TreeStructure {
  def apply[T <: Tree]: Structure[T] = {
    Structure(x => s(x.productPrefix, "(", {
      def default = {
        def showRaw(x: Any): String = x match {
          case el: String => enquote(el, DoubleQuotes)
          case el: Tree => el.show[Structure]
          case el: Nil.type => "Nil"
          case el @ Seq(Seq()) => "Seq(Seq())"
          case el: Seq[_] => "Seq(" + el.map(showRaw).mkString(", ") + ")"
          case el: None.type => "None"
          case el: Some[_] => "Some(" + showRaw(el.get) + ")"
          case el => el.toString
        }
        r(x.productIterator.map(showRaw).toList, ", ")
      }
      x match {
        case x: Quasi =>
          default
        case x @ Lit(value: String) =>
          s(enquote(value, DoubleQuotes))
        case x @ Lit(_) =>
          def isRelevantToken(tok: Token) = tok match {
            case Constant.Int(_) => true
            case Constant.Long(_) => true
            case Constant.Float(_) => true
            case Constant.Double(_) => true
            case Constant.Char(_) => true
            case Constant.Symbol(_) => true
            case Constant.String(_) => true
            case True() => true
            case False() => true
            case Null() => true
            case Ident("-") => true
            case _ => false
          }
          s(x.tokens.filter(isRelevantToken).map(_.show[Syntax]).mkString)
        case x =>
          default
      }
    }, ")"))
  }
}