package scala.meta
package internal
package prettyprinters

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.collections._
import scala.meta.classifiers._
import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.ast.Quasi

object TreeStructure {
  def apply[T <: Tree](implicit options: Options): Structure[T] = {
    Structure(x => s(x.productPrefix, "(", {
      def default = {
        def anyStructure(x: Any): String = x match {
          case el: String => enquote(el, DoubleQuotes)
          case el: Tree => el.show[Structure]
          case el: Seq[_] => seqStructure(el)
          case el: None.type => "None"
          case el: Some[_] => "Some(" + anyStructure(el.get) + ")"
          case el => el.toString
        }
        def seqStructure(xs: Seq[_]): String = {
          if (options.isLazy && xs.isLazy) "Seq(...)"
          else xs match {
            case xs: Nil.type => "Nil"
            case xs @ Seq(Seq()) => "Seq(Seq())"
            case xs => "Seq(" + xs.map(anyStructure).mkString(", ") + ")"
          }
        }
        r(x.productIterator.map(anyStructure).toList, ", ")
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
            case KwTrue() => true
            case KwFalse() => true
            case KwNull() => true
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