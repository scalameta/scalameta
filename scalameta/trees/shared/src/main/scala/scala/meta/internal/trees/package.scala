package scala.meta
package internal

import scala.meta.classifiers._
import scala.meta.internal.tokens.Chars

import java.lang.{Character => JCharacter}

import scala.annotation.switch
import scala.annotation.tailrec
import scala.reflect.ClassTag

package object trees {

  implicit class XtensionTreesRef(private val ref: Ref) extends AnyVal {
    def isWithin: Boolean = ref match {
      case _: Ref.Quasi => true
      case _: Name => true
      case Term.This(_: Name.Anonymous) => true
      case _ => false
    }
  }

  implicit class XtensionTreesName(private val name: Name) extends AnyVal {
    def isDefinition: Boolean = name.parent match {
      case Some(parent: Member) => parent.name == name
      case _ => false
    }
    def isReference: Boolean = !isDefinition

    // some heuristic is needed to govern associativity and precedence of unquoted operators
    def isLeftAssoc: Boolean = name.is[Name.Quasi] || name.value.isLeftAssoc

    // opPrecedence?
    def precedence: Int = if (name.is[Name.Quasi]) 1 else name.value.precedence

    def isUnaryOp: Boolean = !name.is[Name.Quasi] && name.value.isUnaryOp

    def isAssignmentOp: Boolean = !name.is[Name.Quasi] && name.value.isAssignmentOp
  }

  implicit class XtensionTreesString(private val value: String) extends AnyVal {
    import XtensionTreesString._

    // some heuristic is needed to govern associativity and precedence of unquoted operators
    def isLeftAssoc: Boolean = value.last != ':'

    def isUnaryOp: Boolean = Unary.opMap.contains(value)

    def isAssignmentOp = value match {
      case "!=" | "<=" | ">=" | "" => false
      case _ => value.last == '=' && value.head != '=' && Chars.isOperatorPart(value.head)
    }

    // opPrecedence?
    def precedence: Int =
      if (isAssignmentOp) 0
      else if (isScalaLetter(value.head)) 1
      else (value.head: @switch) match {
        case '|' => 2
        case '^' => 3
        case '&' => 4
        case '=' | '!' => 5
        case '<' | '>' => 6
        case ':' => 7
        case '+' | '-' => 8
        case '*' | '/' | '%' => 9
        case _ => 10
      }
  }

  object XtensionTreesString {
    private final val otherLetters = Set[Char]('\u0024', '\u005F') // '$' and '_'
    private final val letterGroups = {
      import JCharacter._
      Set[Byte](LOWERCASE_LETTER, UPPERCASE_LETTER, OTHER_LETTER, TITLECASE_LETTER, LETTER_NUMBER)
    }
    private def isScalaLetter(ch: Char) = letterGroups(JCharacter.getType(ch).toByte) ||
      otherLetters(ch)
  }

  implicit class XtensionTreesTerm(private val tree: Term) extends AnyVal {
    @tailrec
    def isExtractor: Boolean = tree match {
      case quasi: Term.Quasi => true
      case ref: Term.Ref => ref.isPath
      case t: Term.ApplyType => t.fun.isExtractor
      case _ => false
    }
  }

  implicit class XtensionTreesTermRef(private val tree: Term.Ref) extends AnyVal {
    @tailrec
    def isQualId: Boolean = tree match {
      case _: Term.Ref.Quasi => true
      case _: Term.Name => true
      case Term.Select(qual: Term.Ref, _) => qual.isQualId
      case _ => false
    }
    @tailrec
    def isPath: Boolean = tree match {
      case _: Term.Ref.Quasi | _: Term.This | _: Term.Name | _: Term.Anonymous => true
      case Term.Select(_: Term.Super | _: Term.Quasi, _) => true
      case Term.Select(qual: Term.Ref, _) => qual.isPath
      case _ => false
    }
  }

  implicit class XtensionTreesType(private val tree: Type) extends AnyVal {
    def isConstructable: Boolean = tree match {
      case _: Type.Quasi => true
      case _: Type.Name => true
      case _: Type.Select => true
      case _: Type.Project => true
      case _: Type.Function => true
      case _: Type.ContextFunction => true
      case _: Type.Annotate => true
      case _: Type.Apply => true
      case _: Type.ApplyInfix => true
      case _: Type.Refine => true
      case _: Type.AnonymousLambda => true
      case Type.Singleton(Term.This(_: Name.Anonymous)) => true
      case _ => false
    }
  }

  implicit class XtensionHelpersMod(private val mod: Mod) extends AnyVal {
    def isAccessMod: Boolean = mod match {
      case _: Mod.Private => true
      case _: Mod.Protected => true
      case _ => false
    }
    def accessBoundary: Option[Ref] = mod match {
      case m: Mod.WithWithin => m.within match {
          case r: Name if r.value.isEmpty => None
          case r => Some(r)
        }
      case _ => None
    }
    def isNakedAccessMod: Boolean = isAccessMod && accessBoundary.isEmpty
    def isQualifiedAccessMod: Boolean = isAccessMod && accessBoundary.nonEmpty
  }

  implicit class XtensionTreesMods(private val mods: collection.Iterable[Mod]) extends AnyVal {
    def has[T <: Mod](implicit tag: ClassTag[T]): Boolean = mods.exists { case m =>
      tag.runtimeClass.isAssignableFrom(m.getClass)
    }
    def first[T <: Mod](implicit tag: ClassTag[T]): Option[T] = mods
      .collectFirst { case m if tag.runtimeClass.isAssignableFrom(m.getClass) => m.asInstanceOf[T] }
  }

  implicit class XtensionTreesStat(private val stat: Stat) extends AnyVal {
    def isTopLevelStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Export => true
      case _: Pkg => true
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _: Defn.Object => true
      case _: Defn.Def => true
      case _: Defn.Given => true
      case _: Defn.GivenAlias => true
      case _: Defn.Enum => true
      case _: Defn.ExtensionGroup => true
      case _: Defn.Val => true
      case _: Defn.Type => true
      case _: Decl.Type => true
      case _: Term.EndMarker => true
      case _: Defn.Var => true
      case _: Pkg.Object => true
      case _ => false
    }
    def isTemplateStat: Boolean = stat match {
      case _: Stat.Quasi => true
      case _: Import => true
      case _: Export => true
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
      case _: Decl.Type => true
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

  implicit class XtensionTreesCase(private val tree: Case) extends AnyVal {
    def stats: List[Stat] = tree.body match {
      case Term.Block(stats) => stats
      case body => List(body)
    }
  }

  @tailrec
  def arrayClass(clazz: Class[_], rank: Int): Class[_] = {
    import scala.runtime.ScalaRunTime
    Predef.require(rank >= 0)
    if (rank == 0) clazz else arrayClass(ScalaRunTime.arrayClass(clazz), rank - 1)
  }

  private[meta] def checkValidParamClauses(paramClauses: Iterable[Term.ParamClause]): Boolean = {
    var hadImplicit = false
    !paramClauses.exists { pc =>
      hadImplicit || !pc.is[Quasi] && {
        hadImplicit = pc.mod.is[Mod.Implicit]
        var hadRepeated = false
        pc.values.exists { v =>
          try hadRepeated
          finally hadRepeated = !v.is[Quasi] && v.decltpe.is[Type.Repeated]
        }
      }
    }
  }

  private[meta] def checkValidEnumerators(enums: List[Enumerator]): Boolean =
    !enums.headOption.isOpt[Enumerator.Guard]

}
