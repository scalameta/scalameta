package scala.meta
package internal
package trees

import scala.annotation.tailrec

// NOTE: Brings up great memories of thousands of lines of SyntacticXXX in scala.reflect.
// It looks like we can't completely get away from this unsatisfying pattern even in scala.meta.
// See #277 and #405 for details.
object Syntactic {
  object TermApply {
    object ArgList {
      def apply(fun: Term, argss: List[Term.ArgClause])(implicit dialect: Dialect): Term = argss
        .foldLeft(fun)((curr, args) => Term.Apply(curr, args))

      def unapply(tree: Tree): Option[(Term, List[Term.ArgClause])] = tree match {
        case term: Term => Some(unapplyImpl(term, Nil))
        case _ => None
      }

      @tailrec
      private final def unapplyImpl(
          tree: Term,
          prev: List[Term.ArgClause]
      ): (Term, List[Term.ArgClause]) = tree match {
        case t: Term.Apply => unapplyImpl(t.fun, t.argClause :: prev)
        case _ => (tree, prev)
      }
    }

    object ArgListList {
      def apply(fun: Term, argss: List[List[Term]])(implicit dialect: Dialect): Term =
        ArgList(fun, argss.map(Term.ArgClause(_, None)))

      def unapply(tree: Tree): Option[(Term, List[List[Term]])] = tree match {
        case term: Term => Some(unapplyImpl(term, Nil))
        case _ => None
      }

      @tailrec
      private final def unapplyImpl(tree: Term, prev: List[List[Term]]): (Term, List[List[Term]]) =
        tree match {
          case t: Term.Apply => unapplyImpl(t.fun, t.argClause.values :: prev)
          case _ => (tree, prev)
        }
    }
  }
}
