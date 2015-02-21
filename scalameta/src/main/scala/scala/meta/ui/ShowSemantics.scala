package scala.meta
package ui

import org.scalameta.show._
import org.scalameta.invariants._
import org.scalameta.unreachable
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.ast._
import scala.{meta => api}
import scala.meta.syntactic.Token
import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.meta.internal.hygiene._
import scala.compat.Platform.EOL

@implicitNotFound(msg = "don't know how to show[Semantics] for ${T}")
trait Semantics[T] extends Show[T]
object Semantics {
  def apply[T](f: T => Show.Result): Semantics[T] = new Semantics[T] { def apply(input: T) = f(input) }

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def semanticsTree[T <: api.Tree]: Semantics[T] = Semantics(x => {
    var _nextId = 1
    def nextId() = { val id = _nextId; _nextId = _nextId + 1; id }
    val denots = mutable.Map[Denotation, Int]()
    def id(x: Denotation): Int = denots.getOrElseUpdate(x, nextId())
    def body(x: api.Tree): String = {
      def whole(x: Any): String = x match {
        case x: String => enquote(x, DoubleQuotes)
        case x: api.Tree => body(x)
        case x: Nil.type => "Nil"
        case x: List[_] => "List(" + x.map(whole).mkString(", ") + ")"
        case x: None.type => "None"
        case x: Some[_] => "Some(" + whole(x.get) + ")"
        case x => x.toString
      }
      def contents(x: api.Tree): String = x match {
        case x: Lit.String => enquote(x.value, DoubleQuotes)
        case x: Lit => import scala.meta.dialects.Scala211; x.show[Code]
        case x => x.productIterator.map(whole).mkString(", ")
      }
      val syntax = x.productPrefix + "(" + contents(x) + ")"
      val semantics = x match {
        case x: Name =>
          (x.denot, x.sigma) match {
            case (denot: Denotation.Precomputed, Sigma.Naive) => s"[${id(denot)}]"
            case (Denotation.Zero, Sigma.Zero) => "[0]"
            case _ => unreachable
          }
        case _ => ""
      }
      syntax + semantics
    }
    def footer(): String = {
      def prettyprintPrefix(pre: Prefix): String = {
        pre match {
          case Prefix.Zero => "0"
          case Prefix.Type(tpe) => body(tpe)
        }
      }
      def prettyprintSymbol(sym: Symbol): String = {
        def loop(sym: Symbol): String = sym match {
          case Symbol.Zero => "0"
          case Symbol.Root => "_root_"
          case Symbol.Empty => "_empty_"
          case Symbol.Global(owner, name, Signature.Type) => loop(owner) + "#" + name
          case Symbol.Global(owner, name, Signature.Term) => loop(owner) + "." + name
          case Symbol.Global(owner, name, Signature.Method(jvmSignature)) => loop(owner) + "." + name + jvmSignature
          case Symbol.Global(owner, name, Signature.TypeParameter) => loop(owner) + "[" + name + "]"
          case Symbol.Global(owner, name, Signature.TermParameter) => loop(owner) + "(" + name + ")"
          case Symbol.Local(id) => "local#" + id
        }
        var result = loop(sym)
        if (result != "_root_") result = result.stripPrefix("_root_.")
        result
      }
      def prettyprintDenotation(denot: Denotation): String = {
        prettyprintPrefix(denot.prefix) + "::" + prettyprintSymbol(denot.symbol)
      }
      var prevSize = 0
      do { prevSize = denots.size; denots.keys.toList.foreach(prettyprintDenotation) }
      while (denots.size != prevSize)
      if (denots.isEmpty) ""
      else EOL + denots.toList.sortBy{ case (k, v) => v }.map{ case (k, v) => s"[$v] ${prettyprintDenotation(k)}" }.mkString(EOL)
    }
    s(body(x) + footer())
  })
}
