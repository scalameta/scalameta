package scala.meta
package internal
package trees

import scala.meta.classifiers._

import scala.annotation.tailrec

object ParentChecks {
  @tailrec
  private[meta] def termArgument(parent: Tree, destination: String): Boolean = parent match {
    case _: Term.ArgClause => true
    case _: Term.Interpolate => destination == "args"
    case _: Term.ApplyUnary => destination == "arg"
    case _: Term.Assign => destination == "rhs"
    case _: Term.Block => parent.parent match {
        case Some(p) => termArgument(p, destination)
        case None => true
      }
    case _ => false
  }

  private[meta] def typeArgument(tree: Type, destination: String): Boolean = tree.parent.exists {
    case _: Term.Param => destination == "decltpe"
    case _: Type.FuncParamClause => true
    case _: Type.Capturing => true
    case _: Type.ByNameType => !tree.is[Type.ByNameType] && destination == "tpe"
    case _: Type.Repeated => !tree.is[Type.Repeated] && destination == "tpe"
    case _: Type.FunctionArg => !tree.is[Type.FunctionArg] && destination == "tpe"
    case _ => false
  }

  private[meta] def EnumCase(tree: Tree, destination: String): Boolean = tree.parent match {
    case Some(p: Template.Body) => p.parent.parent.isOpt[Defn.Enum]
    case _ => false
  }

  private[meta] def MemberTuple(args: List[Tree]): Boolean = args match {
    case Member.Tuple(_ :: Nil) :: Nil => false
    case _ => true
  }

}
