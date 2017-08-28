package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r }
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.trees.Quasi

object TreeStructure {
  def apply[T <: Tree]: Structure[T] = {
    Structure {
      case Name.Anonymous() =>
        s("Name(\"\")")
      case Name.Indeterminate(value) =>
        s("Name(", enquote(value, DoubleQuotes), ")")
      case x =>
        s(x.productPrefix, "(", {
          def default = {
            def anyStructure(x: Any): String = x match {
              case el: String => enquote(el, DoubleQuotes)
              case el: Tree => el.structure
              case el: List[_] => listStructure(el)
              case el: None.type => "None"
              case el: Some[_] => "Some(" + anyStructure(el.get) + ")"
              case el => el.toString
            }
            def listStructure(xs: List[_]): String = xs match {
              case xs: Nil.type => "Nil"
              case xs @ List(List()) => "List(List())"
              case xs => "List(" + xs.map(anyStructure).mkString(", ") + ")"
            }
            r(x.productIterator.map(anyStructure).toList, ", ")
          }
          x match {
            case x: Quasi =>
              default
            case x @ Lit(value: String) =>
              s(enquote(value, DoubleQuotes))
            case x @ Lit(()) =>
              s("()")
            case x @ Lit.Double(_) =>
              s(x.tokens.mkString)
            case x @ Lit.Float(_) =>
              s(x.tokens.mkString)
            case x @ Lit(_) =>
              def isRelevantToken(tok: Token) = tok match {
                case Constant.Int(_) => true
                case Constant.Long(_) => true
                case Constant.Char(_) => true
                case Constant.Symbol(_) => true
                case Constant.String(_) => true
                case KwTrue() => true
                case KwFalse() => true
                case KwNull() => true
                case Ident("-") => true
                case _ => false
              }
              def showToken(tok: Token) = tok match {
                case Constant.Long(v) => Show.Str(v.toString + "L")
                case _ => tok.syntax
              }
              s(x.tokens.filter(isRelevantToken _).map(showToken _).mkString)
            case Name.Indeterminate(value) =>
              s(enquote(value, DoubleQuotes))
            case Name.Anonymous() =>
              s(enquote("", DoubleQuotes))
            case x =>
              default
          }
        }, ")")
    }
  }
}
