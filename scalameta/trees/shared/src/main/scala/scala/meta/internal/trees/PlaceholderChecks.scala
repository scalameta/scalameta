package scala.meta.internal.trees

import scala.meta._

import scala.annotation.tailrec
import scala.collection.mutable

object PlaceholderChecks {

  def isPlaceholder(tree: Tree): Boolean = tree match {
    case _: Term.Placeholder => true
    case Term.Ascribe(_: Term.Placeholder, _) => true
    case Term.Repeated(_: Term.Placeholder) => true
    case t: Term.Annotate => t.expr.isInstanceOf[Term.Placeholder]
    case _ => false
  }

  def isAnonymousParam(tree: Tree): Boolean = tree match {
    case _: Type.AnonymousParam => true
    case _ => false
  }

  def isBlockPlaceholder(args: List[Tree]): Boolean = args match {
    case List(arg) => isBlockPlaceholder(arg)
    case _ => false
  }

  def isBlockPlaceholder(tree: Tree): Boolean = tree match {
    case Term.Block(List(arg)) => isPlaceholder(arg)
    case _ => false
  }

  def hasPlaceholder(tree: Tree, includeArg: => Boolean): Boolean = {
    val queue = new mutable.Queue[Tree]
    @tailrec
    def iter: Boolean = queue.dequeue() match {
      case _: Quasi => queue.nonEmpty && iter
      case t if isPlaceholder(t) => t.ne(tree) || includeArg
      case t: Term.SelectLike =>
        queue += t.qual
        iter
      case t: Term.Tuple => t.args.exists(isPlaceholder) || queue.nonEmpty && iter
      case t: Init => t.argClauses
          .exists(x => !x.isInstanceOf[Quasi] && x.values.exists(isPlaceholder)) ||
        queue.nonEmpty && iter
      case t: Term.Apply =>
        queue += t.fun
        queue += t.argClause
        iter
      case t: Term.ArgClause => isBlockPlaceholder(t.values) ||
        (t.parent match {
          case Some(_: Term.ApplyInfix) =>
            queue ++= t.values
            queue.nonEmpty && iter
          case _ => t.values match {
              /* - In `f { case ... if _ => ... }`, `f {}` is an anonymous function.
               * - In `f({ case ... if _ => ... })`, arg is an anonymous function,
               *   i.e. cannot be a partial function by now. */
              case (v: Term.PartialFunction) :: Nil =>
                queue += v
                iter
              case vs => vs.exists(isPlaceholder) || queue.nonEmpty && iter
            }
        })
      case t: Term.ApplyInfix =>
        queue += t.lhs
        queue += t.argClause
        iter
      case t: Term.ApplyUnary =>
        queue += t.arg
        iter
      case t: Term.ApplyType =>
        queue += t.fun
        iter
      case t: Term.New =>
        queue += t.init
        iter
      case t: Term.AnonymousFunction => t.ne(tree) && queue.nonEmpty && iter
      case t: Term.Assign => isPlaceholder(t.rhs) || {
          queue += t.lhs
          iter
        }
      case t: Term.Ascribe =>
        queue += t.expr
        iter
      case t: Term.Annotate =>
        queue += t.expr
        iter
      // a selector and a guard are postfix expressions
      case t: Term.MatchLike =>
        queue += t.expr
        queue += t.casesBlock
        iter
      case t: Tree.CasesBlock =>
        queue ++= t.cases
        iter
      case t: Case =>
        t.cond match {
          case Some(cond) => queue += cond
          case _ =>
        }
        t.body match {
          case body: Term.SubMatch => queue += body
          case _ =>
        }
        queue.nonEmpty && iter
      // we search all of a guard, a postfix expression, but elsewhere only a bare `_`
      case t: Term.ForClause => isPlaceholder(t.body) || {
          queue += t.enumsBlock
          iter
        }
      case t: Term.EnumeratorsBlock =>
        queue ++= t.enums
        iter
      case t: Enumerator.Assign => isPlaceholder(t.rhs) || queue.nonEmpty && iter
      case t: Enumerator.Guard =>
        queue += t.cond
        iter
      case t => t.children.exists(isPlaceholder) || queue.nonEmpty && iter
    }
    queue += tree
    iter
  }

  def hasAnonymousParam(tree: Type, includeArg: => Boolean): Boolean = {
    val queue = new mutable.Queue[Tree]
    @tailrec
    def iter: Boolean = queue.dequeue() match {
      case _: Quasi => queue.nonEmpty && iter
      case t: Type.AnonymousParam => t.ne(tree) || includeArg
      case t: Type.Tuple => t.args.exists(isAnonymousParam) || queue.nonEmpty && iter
      case t: Type.ArgClause => t.values.exists(isAnonymousParam) || queue.nonEmpty && iter
      case t: Type.Apply =>
        queue += t.tpe
        queue += t.argClause
        iter
      case t: Type.ApplyInfix =>
        queue += t.lhs
        queue += t.rhs
        iter
      case t: Type.With =>
        queue += t.lhs
        queue += t.rhs
        iter
      case t: Type.Repeated =>
        queue += t.tpe
        iter
      case t => t.children.exists(isAnonymousParam) || queue.nonEmpty && iter
    }
    queue += tree
    iter
  }

}
