package scala.meta
package internal
package trees

import org.scalameta.invariants._
import scala.meta.classifiers._

import scala.annotation.tailrec

object ParentChecks {
  @tailrec
  private def termArgument(parent: Tree, destination: String): Boolean = parent match {
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

  def TermAssign(tree: Term.Assign, parent: Tree, destination: String): Boolean =
    !tree.rhs.is[Term.Repeated] || termArgument(parent, destination)

  def TermRepeated(tree: Term.Repeated, parent: Tree, destination: String): Boolean = parent match {
    case _: Term.Tuple => destination == "args"
    case _ => termArgument(parent, destination)
  }

  def PatVar(tree: Pat.Var, parent: Tree, destination: String): Boolean = parent match {
    case _: Pat.Bind => true
    case _: Decl.Val | _: Decl.Var | _: Defn.Val | _: Defn.Var => destination == "pats"
    case _: Enumerator.Assign => destination == "pat"
    case _ =>
      val value = tree.name.value
      value.isEmpty || !value(0).isUpper
  }

  private def typeArgument(tree: Type, parent: Tree, destination: String): Boolean = parent match {
    case _: Term.Param => destination == "decltpe"
    case _: Type.FuncParamClause => true
    case _: Type.ByName => destination == "tpe"
    case _: Type.Repeated => tree.is[Type.ByName] && destination == "tpe"
    case _ => false
  }

  def TypeByName(tree: Type.ByName, parent: Tree, destination: String): Boolean =
    typeArgument(tree, parent, destination)

  def TypeRepeated(tree: Type.Repeated, parent: Tree, destination: String): Boolean =
    typeArgument(tree, parent, destination)

  def PatSeqWildcard(tree: Pat.SeqWildcard, parent: Tree, destination: String): Boolean =
    parent match {
      case _: Pat.Bind => destination == "rhs"
      case _: Pat.ArgClause => true
      case _: Pat.Interpolate | _: Pat.Xml => destination == "args"
      case _ => false
    }

  def AnonymousImport(tree: Term.Anonymous, parent: Tree, destination: String): Boolean = parent
    .is[Importer]

  def NameAnonymous(tree: Name.Anonymous, parent: Tree, destination: String): Boolean =
    parent match {
      case _: Ctor | _: Init | _: Self | _: Term.Param | _: Type.Param => destination == "name"
      case _: Mod.Private | _: Mod.Protected => destination == "within"
      case _: Term.This | _: Term.Super | _: Defn.Given | _: Defn.GivenAlias => true
      case _: Defn.ExtensionGroup | _: Defn.RepeatedEnumCase => true
      case _ => false
    }

  def NameThis(tree: Name.This, parent: Tree, destination: String): Boolean = parent match {
    case _: Ctor.Secondary | _: Self => destination == "name"
    case _ => false
  }

  def NamePlaceholder(tree: Name.Placeholder, parent: Tree, destination: String): Boolean =
    parent match {
      case _: Term.Param | _: Type.Param | _: Self => destination == "name"
      case _ => false
    }

  def TypeVar(tree: Type.Var, parent: Tree, destination: String): Boolean = {
    @tailrec
    def loop(tree: Option[Tree]): Boolean = tree match {
      case Some(tree: Type) => loop(tree.parent)
      case Some(tree: Type.ArgClause) => loop(tree.parent)
      case Some(_: Pat.Typed) => true
      case Some(tree: Term.ApplyType) => tree.parent.isOpt[Pat.Extract]
      case Some(_) => false
      case None => true
    }
    loop(Some(tree))
  }

  def Init(tree: Init, parent: Tree, destination: String): Boolean = tree.tpe.is[Type.Singleton] ==>
    (parent.is[Ctor.Block] && destination == "init")

  def EnumCase(tree: Tree, parent: Tree, destination: String): Boolean = parent.is[Template.Body] &&
    parent.parent.parent.isOpt[Defn.Enum]

  def TypeLambda(tree: Type.Lambda, parent: Tree, destination: String): Boolean = parent.is[Type] ||
    parent.is[Defn.Type] || parent.is[Type.Bounds] || parent.is[Term.ApplyType] ||
    parent.is[Type.Param] || parent.is[Type.ArgClause]

  def TypeMethod(tree: Type.Method, parent: Tree, destination: String): Boolean = parent.is[Type] ||
    parent.is[Defn.Type]

  def TermBlock(tree: Term.Block, parent: Tree, dest: String): Boolean = tree.stats.forall {
    case _: Decl => true
    case _: Export => parent.is[Defn.ExtensionGroup]
    case x => x.isBlockStat
  }

  private[meta] def MemberTuple(args: List[Tree]): Boolean = args match {
    case Member.Tuple(_ :: Nil) :: Nil => false
    case _ => true
  }

}
