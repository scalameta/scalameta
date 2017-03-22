package scala.meta
package internal
package ast

import scala.{Seq => _}
import scala.collection.immutable.Seq

// NOTE: Brings up great memories of thousands of lines of SyntacticXXX in scala.reflect.
// It looks like we can't completely get away from this unsatisfying pattern even in scala.meta.
// See #277 and #405 for details.
object Syntactic {
  object Term {
    object Apply {
      def apply(fun: scala.meta.Term, argss: Seq[Seq[scala.meta.Term.Arg]]): scala.meta.Term = {
        argss.foldLeft(fun)((curr, args) => scala.meta.Term.Apply(curr, args))
      }

      def apply(fun: scala.meta.Ctor.Call, argss: Seq[Seq[scala.meta.Term.Arg]]): scala.meta.Ctor.Call = {
        argss.foldLeft(fun)((curr, args) => scala.meta.Term.Apply(curr, args))
      }

      def unapply(tree: scala.meta.Tree): Option[(scala.meta.Term, Seq[Seq[scala.meta.Term.Arg]])] = {
        tree match {
          case scala.meta.Term.Apply(Syntactic.Term.Apply(core, argss), args) => Some((core, argss :+ args))
          case tree: scala.meta.Term => Some((tree, Nil))
          case _ => None
        }
      }
    }
  }
}
