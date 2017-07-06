package scala.meta
package internal
package trees

// NOTE: Brings up great memories of thousands of lines of SyntacticXXX in scala.reflect.
// It looks like we can't completely get away from this unsatisfying pattern even in scala.meta.
// See #277 and #405 for details.
object Syntactic {
  object Term {
    object Apply {
      def apply(fun: scala.meta.Term, argss: List[List[scala.meta.Term]]): scala.meta.Term = {
        argss.foldLeft(fun)((curr, args) => scala.meta.Term.Apply(curr, args))
      }

      def unapply(tree: scala.meta.Tree): Option[(scala.meta.Term, List[List[scala.meta.Term]])] = {
        tree match {
          case scala.meta.Term.Apply(Syntactic.Term.Apply(core, argss), args) => Some((core, argss :+ args))
          case tree: scala.meta.Term => Some((tree, Nil))
          case _ => None
        }
      }
    }
  }
}
