package scala.meta
package internal
package ast

import java.lang.{ Character => JCharacter }
import scala.reflect.ClassTag
import org.scalameta._
import org.scalameta.invariants._
import scala.annotation.switch
import scala.meta.classifiers._
import scala.meta.prettyprinters._

object Helpers {
  val unaryOps = Set("-", "+", "~", "!")
  def isUnaryOp(s: String): Boolean = unaryOps contains s

  implicit class XtensionHelpersName(name: Name) {
    def isBinder: Boolean = name.parent match {
      case Some(parent: Member) => parent.name == name
      case _ => false
    }
    def isReference: Boolean = !isBinder
  }

  implicit class XtensionHelpersTermName(name: Term.Name) {
    import name._
    // some heuristic is needed to govern associativity and precedence of unquoted operators
    def isLeftAssoc: Boolean = if (name.is[Term.Name.Quasi]) true
                               else value.last != ':'
    def isUnaryOp: Boolean = Helpers.isUnaryOp(value)
    def isAssignmentOp = value match {
      case "!=" | "<=" | ">=" | "" => false
      case _                       => (value.last == '=' && value.head != '='
                                       && isOperatorPart(value.head))
    }
    // opPrecedence?
    def precedence: Int =
      if (name.is[Term.Name.Quasi]) 1
      else if (isAssignmentOp) 0
      else if (isScalaLetter(value.head)) 1
      else (value.head: @scala.annotation.switch) match {
        case '|'             => 2
        case '^'             => 3
        case '&'             => 4
        case '=' | '!'       => 5
        case '<' | '>'       => 6
        case ':'             => 7
        case '+' | '-'       => 8
        case '*' | '/' | '%' => 9
        case _               => 10
      }

    // TODO: deduplicate with Chars.scala in tokenizers
    private final val otherLetters = Set[Char]('\u0024', '\u005F')  // '$' and '_'
    private final val letterGroups = {
      import JCharacter._
      Set[Byte](LOWERCASE_LETTER, UPPERCASE_LETTER, OTHER_LETTER, TITLECASE_LETTER, LETTER_NUMBER)
    }
    private def isScalaLetter(ch: Char) = letterGroups(JCharacter.getType(ch).toByte) || otherLetters(ch)

    /** Is character a math or other symbol in Unicode?  */
    private def isSpecial(c: Char) = {
      val chtp = Character.getType(c)
      chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
    }

    /** Can character form part of a Scala operator name? */
    private def isOperatorPart(c : Char) : Boolean = (c: @switch) match {
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '/' | '\\' => true
      case c => isSpecial(c)
    }
  }

  implicit class XtensionHelpersTerm(tree: Term) {
    def isExtractor: Boolean = tree match {
      case quasi: Term.Quasi => true
      case ref: Term.Ref => ref.isStableId
      case Term.ApplyType(tree, _) => tree.isExtractor
      case _ => false
    }
  }

  implicit class XtensionHelpersTermRef(tree: Term.Ref) {
    def isPath: Boolean = tree.isStableId || tree.is[Term.This]
    def isQualId: Boolean = tree match {
      case _: Term.Ref.Quasi              => true
      case _: Term.Name                   => true
      case Term.Select(qual: Term.Ref, _) => qual.isQualId
      case _                              => false
    }
    def isStableId: Boolean = tree match {
      case _: Term.Ref.Quasi              => true
      case _: Term.Name | Term.Select(_: Term.Super, _) => true
      case Term.Select(qual: Term.Quasi, _)             => true
      case Term.Select(qual: Term.Ref, _)               => qual.isPath
      case _                                            => false
    }
  }

  implicit class XtensionHelpersType(tree: Type) {
    def isConstructable: Boolean = tree match {
      case _: Type.Quasi                               => true
      case _: Type.Name                                => true
      case _: Type.Select                              => true
      case _: Type.Project                             => true
      case _: Type.Function                            => true
      case _: Type.Annotate                            => true
      case _: Type.Apply                               => true
      case _: Type.ApplyInfix                          => true
      case Type.Singleton(Term.This(Name.Anonymous())) => true
      case _                                           => false
    }
  }

  implicit class XtensionHelpersMod(mod: Mod) {
    def isAccessMod: Boolean = mod match {
      case _: Mod.PrivateThis => true
      case _: Mod.PrivateWithin => true
      case _: Mod.ProtectedThis => true
      case _: Mod.ProtectedWithin => true
      case _ => false
    }
    def accessBoundary: Option[Ref] = mod match {
      case Mod.PrivateThis() => Some(Term.This(Name.Anonymous()))
      case Mod.ProtectedThis() => Some(Term.This(Name.Anonymous()))
      case Mod.PrivateWithin(Name.Anonymous()) => None
      case Mod.ProtectedWithin(Name.Anonymous()) => None
      case Mod.PrivateWithin(name) => Some(name)
      case Mod.ProtectedWithin(name) => Some(name)
      case _ => None
    }
    def isNakedAccessMod: Boolean = isAccessMod && accessBoundary.isEmpty
    def isQualifiedAccessMod: Boolean = isAccessMod && accessBoundary.nonEmpty
  }

  implicit class XtensionHelpersMods(mods: List[Mod]) {
    def has[T <: Mod](implicit classifier: Classifier[Mod, T]): Boolean =
      mods.exists(classifier.apply)
    def getAll[T <: Mod](implicit tag: ClassTag[T],
                         classifier: Classifier[Mod, T]): List[T] =
      mods.collect { case m if classifier.apply(m) => m.require[T] }
    def getIncompatible[T <: Mod, U <: Mod]
      (implicit classifier1: Classifier[Mod, T], tag1: ClassTag[T],
                classifier2: Classifier[Mod, U], tag2: ClassTag[U]): List[(Mod, Mod)] =
      getAll[T].zip(getAll[U])
  }

  implicit class XtensionHelpersStat(stat: Stat) {
    def isTopLevelStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Pkg => true
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _: Defn.Object => true
      case _: Pkg.Object => true
      case _ => false
    }
    def isTemplateStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Term => true
      case _: Decl => true
      case _: Defn => true
      case _: Ctor.Secondary => true
      case _ => false
    }
    def isBlockStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Term => true
      case _: Defn => true
      case _ => false
    }
    def isRefineStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Decl => true
      case _: Defn.Type => true
      case _ => false
    }
    def isExistentialStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Decl.Val => true
      case _: Decl.Type => true
      case _ => false
    }
    def isEarlyStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Defn.Val => true
      case _: Defn.Var => true
      case _: Defn.Type => true
      case _ => false
    }
  }

  implicit class XtensionHelpersCase(tree: Case) {
    def stats: List[Stat] = tree.body match {
      case Term.Block(stats) => stats
      case body => List(body)
    }
  }

  def arrayClass(clazz: Class[_], rank: Int): Class[_] = {
    import scala.runtime.ScalaRunTime
    Predef.require(rank >= 0)
    if (rank == 0) clazz
    else arrayClass(ScalaRunTime.arrayClass(clazz), rank - 1)
  }

  object ParentChecks {
    private def termArgument(parent: Tree, destination: String): Boolean = {
      def applyArgument = parent.is[Term.Apply] && destination == "args"
      def applyInfixArgument = parent.is[Term.ApplyInfix] && destination == "args"
      def namedArg = parent.is[Term.Assign] && destination == "rhs"
      applyArgument || applyInfixArgument || namedArg
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
      def declValPat = parent.is[Decl.Val] && destination == "pats"
      def declVarPat = parent.is[Decl.Var] && destination == "pats"
      def defnValPat = parent.is[Defn.Val] && destination == "pats"
      def defnVarPat = parent.is[Defn.Var] && destination == "pats"
      def enumeratorGeneratorPat = parent.is[Enumerator.Generator] && destination == "pat"
      def enumeratorValPat = parent.is[Enumerator.Val] && destination == "pat"
      !capitalized || declValPat || declVarPat || defnValPat || defnVarPat || enumeratorGeneratorPat || enumeratorValPat
    }

    private def typeArgument(parent: Tree, destination: String): Boolean = {
      def termParamDecltpe = parent.is[Term.Param] && destination == "decltpe"
      def typeFunctionArgument = parent.is[Type.Function] && destination == "args"
      def byNameType = parent.is[Type.ByName] && destination == "tpe"
      termParamDecltpe || typeFunctionArgument || byNameType
    }

    def TypeByName(tree: Type.ByName, parent: Tree, destination: String): Boolean = {
      typeArgument(parent, destination)
    }

    def TypeRepeated(tree: Type.Repeated, parent: Tree, destination: String): Boolean = {
      typeArgument(parent, destination)
    }

    def PatSeqWildcard(tree: Pat.SeqWildcard, parent: Tree, destination: String): Boolean = {
      def bindRhs = parent.is[Pat.Bind] && destination == "rhs"
      def extractArgs = parent.is[Pat.Extract] && destination == "args"
      def extractInfixArgs = parent.is[Pat.ExtractInfix] && destination == "args"
      def extractInterpolate = parent.is[Pat.Interpolate] && destination == "args"
      def extractXml = parent.is[Pat.Xml] && destination == "args"
      bindRhs || extractArgs || extractInfixArgs || extractInterpolate || extractXml
    }

    def NameAnonymous(tree: Name.Anonymous, parent: Tree, destination: String): Boolean = {
      def primaryCtorName = parent.is[Ctor.Primary] && destination == "name"
      def secondaryCtorName = parent.is[Ctor.Secondary] && destination == "name"
      def termParamName = parent.is[Term.Param] && destination == "name"
      def typeParamName = parent.is[Type.Param] && destination == "name"
      def initName = parent.is[Init] && destination == "name"
      def privateWithin = parent.is[Mod.PrivateWithin] && destination == "within"
      def protectedWithin = parent.is[Mod.ProtectedWithin] && destination == "within"
      def thisQualifier = parent.is[Term.This]
      def superQualifier = parent.is[Term.Super]
      primaryCtorName || secondaryCtorName || termParamName || typeParamName ||
      initName || privateWithin || protectedWithin || thisQualifier || superQualifier
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
  }
}
