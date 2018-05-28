package scala.meta
package internal

import java.lang.{ Character => JCharacter }
import scala.reflect.ClassTag
import org.scalameta.invariants._
import scala.annotation.switch
import scala.meta.classifiers._
import scala.meta.internal.trees.Metadata.Ast

package object trees {
  implicit class XtensionTreesName(name: Name) {
    def isDefinition: Boolean = name.parent match {
      case Some(parent: Member) => parent.name == name
      case _ => false
    }
    def isReference: Boolean = !isDefinition
  }

  implicit class XtensionTreesRef(ref: Ref) {
    def isWithin: Boolean = ref match {
      case _: Ref.Quasi => true
      case _: Name => true
      case Term.This(Name.Anonymous()) => true
      case _ => false
    }
  }

  implicit class XtensionTreesTermName(name: Term.Name) {
    import name._
    // some heuristic is needed to govern associativity and precedence of unquoted operators
    def isLeftAssoc: Boolean = if (name.is[Term.Name.Quasi]) true
                               else value.last != ':'
    def isUnaryOp: Boolean = Set("-", "+", "~", "!").contains(value)
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

  implicit class XtensionTreesTerm(tree: Term) {
    def isExtractor: Boolean = tree match {
      case quasi: Term.Quasi => true
      case ref: Term.Ref => ref.isStableId
      case Term.ApplyType(tree, _) => tree.isExtractor
      case _ => false
    }
  }

  implicit class XtensionTreesTermRef(tree: Term.Ref) {
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

  implicit class XtensionTreesType(tree: Type) {
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
      case _: Mod.Private => true
      case _: Mod.Protected => true
      case _ => false
    }
    def accessBoundary: Option[Ref] = mod match {
      case Mod.Private(Name.Anonymous()) => None
      case Mod.Protected(Name.Anonymous()) => None
      case Mod.Private(ref) => Some(ref)
      case Mod.Protected(ref) => Some(ref)
      case _ => None
    }
    def isNakedAccessMod: Boolean = isAccessMod && accessBoundary.isEmpty
    def isQualifiedAccessMod: Boolean = isAccessMod && accessBoundary.nonEmpty
  }

  implicit class XtensionTreesMods(mods: List[Mod]) {
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

  implicit class XtensionTreesStat(stat: Stat) {
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

  implicit class XtensionTreesCase(tree: Case) {
    def stats: List[Stat] = tree.body match {
      case Term.Block(stats) => stats
      case body => List(body)
    }
  }

  def astInfo[T <: Ast : AstInfo]: AstInfo[T] = implicitly[AstInfo[T]]

  def arrayClass(clazz: Class[_], rank: Int): Class[_] = {
    import scala.runtime.ScalaRunTime
    Predef.require(rank >= 0)
    if (rank == 0) clazz
    else arrayClass(ScalaRunTime.arrayClass(clazz), rank - 1)
  }
}
