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
        ???
      }

      def unapply(tree: Tree): Option[(scala.meta.Term, Seq[Seq[scala.meta.Term.Arg]])] = {
        ???
      }
    }
  }
}
