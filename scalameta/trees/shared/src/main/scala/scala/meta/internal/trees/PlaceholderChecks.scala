package scala.meta.internal.trees

import scala.meta._

import scala.annotation.tailrec
import scala.collection.mutable

object PlaceholderChecks {

  def isPlaceholder(tree: Tree): Boolean = tree match {
    case _: Term.Placeholder => true
    case Term.Ascribe(_: Term.Placeholder, _) => true
    case Term.Repeated(_: Term.Placeholder) => true
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
      case t: Term.Select => queue += t.qual; iter
      case t: Term.Tuple => t.args.exists(isPlaceholder) || queue.nonEmpty && iter
      case t: Init => t.argClauses
          .exists(x => !x.isInstanceOf[Quasi] && x.values.exists(isPlaceholder)) ||
        queue.nonEmpty && iter
      case t: Term.Apply => queue += t.fun; queue += t.argClause; iter
      case t: Term.ArgClause => isBlockPlaceholder(t.values) ||
        (t.parent match {
          case Some(_: Term.ApplyInfix) => queue ++= t.values; queue.nonEmpty && iter
          case _ => t.values.exists(isPlaceholder) || queue.nonEmpty && iter
        })
      case t: Term.ApplyInfix => queue += t.lhs; queue += t.argClause; iter
      case t: Term.ApplyUnary => queue += t.arg; iter
      case t: Term.ApplyType => queue += t.fun; iter
      case t: Term.New => queue += t.init; iter
      case t: Term.Repeated => queue += t.expr; iter
      case t: Term.AnonymousFunction => t.ne(tree) && queue.nonEmpty && iter
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
      case t: Type.Apply => queue += t.tpe; queue += t.argClause; iter
      case t: Type.ApplyInfix => queue += t.lhs; queue += t.rhs; iter
      case t: Type.With => queue += t.lhs; queue += t.rhs; iter
      case t: Type.Repeated => queue += t.tpe; iter
      case t => t.children.exists(isAnonymousParam) || queue.nonEmpty && iter
    }
    queue += tree
    iter
  }

}
