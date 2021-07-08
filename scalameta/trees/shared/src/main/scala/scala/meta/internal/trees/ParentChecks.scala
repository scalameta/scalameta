package scala.meta
package internal
package trees

import org.scalameta.invariants._
import scala.meta.classifiers._
import scala.meta.Defn.Given
import scala.meta.Defn.GivenAlias
import scala.meta.Defn.ExtensionGroup

object ParentChecks {
  private def termArgument(parent: Tree, destination: String): Boolean = {
    def applyArgument = parent.is[Term.Apply] && destination == "args"
    def applyInfixArgument = parent.is[Term.ApplyInfix] && destination == "args"
    def applyUnaryArgument = parent.is[Term.ApplyUnary] && destination == "arg"
    def assignArgument = parent.is[Term.Assign] && destination == "rhs"
    def interpolateArgument = parent.is[Term.Interpolate] && destination == "args"
    def xmlArgument = parent.is[Term.Interpolate] && destination == "args"
    def initArgument = parent.is[Init] && destination == "argss"
    def namedArgument = parent.is[Term.Assign] && destination == "rhs"
    applyArgument || applyInfixArgument || applyUnaryArgument || assignArgument ||
    interpolateArgument || xmlArgument || initArgument || namedArgument
  }

  def TermAssign(tree: Term.Assign, parent: Tree, destination: String): Boolean = {
    def namedRepeatedArgument = tree.rhs.is[Term.Repeated]
    !namedRepeatedArgument || termArgument(parent, destination)
  }

  def TermRepeated(tree: Term.Repeated, parent: Tree, destination: String): Boolean = {
    termArgument(parent, destination)
  }

  def PatVar(tree: Pat.Var, parent: Tree, destination: String): Boolean = {
    val Pat.Var(Term.Name(value)) = tree
    def capitalized = value.nonEmpty && value(0).isUpper
    def isInPatternBinding = parent.is[Pat.Bind]
    def declValPat = parent.is[Decl.Val] && destination == "pats"
    def declVarPat = parent.is[Decl.Var] && destination == "pats"
    def defnValPat = parent.is[Defn.Val] && destination == "pats"
    def defnVarPat = parent.is[Defn.Var] && destination == "pats"
    def enumeratorGeneratorPat = parent.is[Enumerator.Generator] && destination == "pat"
    def enumeratorValPat = parent.is[Enumerator.Val] && destination == "pat"
    isInPatternBinding || !capitalized || declValPat || declVarPat || defnValPat || defnVarPat ||
    enumeratorGeneratorPat || enumeratorValPat
  }

  private def typeArgument(tree: Type, parent: Tree, destination: String): Boolean = {
    def termParamDecltpe = parent.is[Term.Param] && destination == "decltpe"
    def typeFunctionArgument =
      (parent.is[Type.Function] || parent.is[Type.ContextFunction]) && destination == "params"
    def byNameType = parent.is[Type.ByName] && destination == "tpe"
    def byNameRepeatedType =
      tree.is[Type.ByName] && parent.is[Type.Repeated] && destination == "tpe"
    termParamDecltpe || typeFunctionArgument || byNameType || byNameRepeatedType
  }

  def TypeByName(tree: Type.ByName, parent: Tree, destination: String): Boolean = {
    typeArgument(tree, parent, destination)
  }

  def TypeRepeated(tree: Type.Repeated, parent: Tree, destination: String): Boolean = {
    typeArgument(tree, parent, destination)
  }

  def PatSeqWildcard(tree: Pat.SeqWildcard, parent: Tree, destination: String): Boolean = {
    def bindRhs = parent.is[Pat.Bind] && destination == "rhs"
    def extractArgs = parent.is[Pat.Extract] && destination == "args"
    def extractInfixArgs = parent.is[Pat.ExtractInfix] && destination == "args"
    def extractInterpolate = parent.is[Pat.Interpolate] && destination == "args"
    def extractXml = parent.is[Pat.Xml] && destination == "args"
    bindRhs || extractArgs || extractInfixArgs || extractInterpolate || extractXml
  }

  def AnonymousImport(tree: Term.Anonymous, parent: Tree, destination: String): Boolean = {
    parent.is[Importer]
  }

  def NameAnonymous(tree: Name.Anonymous, parent: Tree, destination: String): Boolean = {
    def primaryCtorName = parent.is[Ctor.Primary] && destination == "name"
    def secondaryCtorName = parent.is[Ctor.Secondary] && destination == "name"
    def termParamName = parent.is[Term.Param] && destination == "name"
    def typeParamName = parent.is[Type.Param] && destination == "name"
    def initName = parent.is[Init] && destination == "name"
    def selfName = parent.is[Self] && destination == "name"
    def privateWithin = parent.is[Mod.Private] && destination == "within"
    def protectedWithin = parent.is[Mod.Protected] && destination == "within"
    def thisQualifier = parent.is[Term.This]
    def givenName = parent.is[Given] || parent.is[GivenAlias]
    def extensionName = parent.is[ExtensionGroup]
    def repeatedCase = parent.is[Defn.RepeatedEnumCase]
    def superQualifier = parent.is[Term.Super]
    primaryCtorName || secondaryCtorName || termParamName || typeParamName ||
    initName || selfName || privateWithin || protectedWithin || thisQualifier ||
    superQualifier || givenName || extensionName || repeatedCase
  }

  def TypeVar(tree: Type.Var, parent: Tree, destination: String): Boolean = {
    def loop(tree: Option[Tree]): Boolean = tree match {
      case Some(tree: Type) => loop(tree.parent)
      case Some(_: Pat.Typed) => true
      case Some(tree: Term.ApplyType) => tree.parent.map(_.is[Pat.Extract]).getOrElse(true)
      case Some(_) => false
      case None => true
    }
    loop(Some(tree))
  }

  def Init(tree: Init, parent: Tree, destination: String): Boolean = {
    tree.tpe.is[Type.Singleton] ==> (parent.is[Ctor.Secondary] && destination == "init")
  }

  def EnumCase(tree: Tree, parent: Tree, destination: String): Boolean = {
    parent.is[Template] && parent.parent.map(_.is[Defn.Enum]).getOrElse(true)
  }

  def TypeLambda(tree: Type.Lambda, parent: Tree, destination: String): Boolean = {
    parent.is[Type] || parent.is[Defn.Type] || parent.is[Type.Bounds] || parent.is[Term.ApplyType]
  }

  def TypeMethod(tree: Type.Method, parent: Tree, destination: String): Boolean = {
    parent.is[Type] || parent.is[Defn.Type]
  }
}
