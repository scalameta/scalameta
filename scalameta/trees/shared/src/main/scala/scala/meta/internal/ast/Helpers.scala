package scala.meta
package internal
package ast

import java.lang.{ Character => JCharacter }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import org.scalameta._
import org.scalameta.invariants._
import scala.annotation.switch
import scala.meta.classifiers._
import scala.meta.prettyprinters._

object Helpers {
  val unaryOps = Set("-", "+", "~", "!")
  def isUnaryOp(s: String): Boolean = unaryOps contains s

  implicit class XtensionSyntacticName(name: Name) {
    def isBinder: Boolean = name.parent match {
      case Some(parent: Member) => parent.name == name
      case _ => false
    }
    def isReference: Boolean = !isBinder
  }

  implicit class XtensionSyntacticTermName(name: Term.Name) {
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

  implicit class XtensionTermOps(tree: Term) {
    def isCtorCall: Boolean = tree match {
      case _: Term.Quasi => true
      case _: Ctor.Ref => true
      case Term.ApplyType(callee, _) => callee.isCtorCall
      case Term.Apply(callee, _) => callee.isCtorCall
      case Term.Annotate(annottee, _) => annottee.isCtorCall
      case _ => false
    }
    def ctorTpe: Type = {
      def loop(tree: Tree): Type = tree match {
        case Ctor.Ref.Name(value) => Type.Name(value)
        case Ctor.Ref.Select(qual, name) => Type.Select(qual, Type.Name(name.value))
        case Ctor.Ref.Project(qual, name) => Type.Project(qual, Type.Name(name.value))
        case Ctor.Ref.Function(_) => unreachable(debug(XtensionTermOps.this.tree, XtensionTermOps.this.tree.show[Structure]))
        case Term.ApplyType(Ctor.Ref.Function(_), targs) => Type.Function(targs.init, targs.last)
        case Term.ApplyType(callee, targs) => Type.Apply(loop(callee), targs)
        case Term.Apply(callee, _) => callee.ctorTpe
        case Term.Annotate(annottee, annots) => Type.Annotate(loop(annottee), annots)
        case _ => unreachable(debug(XtensionTermOps.this.tree, XtensionTermOps.this.tree.show[Structure], tree, tree.show[Structure]))
      }
      loop(tree)
    }
    def ctorArgss: Seq[Seq[Term]] = {
      def loop(tree: Tree): Seq[Seq[Term]] = tree match {
        case _: Ctor.Ref => Nil
        case Term.ApplyType(callee, _) => callee.ctorArgss
        case Term.Apply(callee, args) => callee.ctorArgss :+ args
        case Term.Annotate(annottee, _) => annottee.ctorArgss
        case _ => unreachable(debug(XtensionTermOps.this.tree, XtensionTermOps.this.tree.show[Structure]))
      }
      loop(tree)
    }
    def isCtorBody: Boolean = {
      def isSuperCall(tree: Tree): Boolean = tree match {
        case _: Ctor.Name => true
        case Term.Apply(fn, _) => isSuperCall(fn)
        case _ => false // you can't write `this[...](...)`
      }
      tree match {
        case _: Term.Quasi => true
        case Term.Block(superCall +: _) => isSuperCall(superCall)
        case superCall => isSuperCall(superCall)
      }
    }
  }

  implicit class XtensionTermRefOps(tree: Term.Ref) {
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

  implicit class XtensionMod(mod: Mod) {
    def isAccessMod: Boolean = mod match {
      case _: Mod.Private => true
      case _: Mod.Protected => true
      case _ => false
    }
    def accessBoundary: Option[Name.Qualifier] = mod match {
      case Mod.Private(Name.Anonymous()) => None
      case Mod.Protected(Name.Anonymous()) => None
      case Mod.Private(name) => Some(name)
      case Mod.Protected(name) => Some(name)
      case _ => None
    }
    def isNakedAccessMod: Boolean = isAccessMod && accessBoundary.isEmpty
    def isQualifiedAccessMod: Boolean = isAccessMod && accessBoundary.nonEmpty
  }

  implicit class XtensionMods(mods: List[Mod]) {
    def has[T <: Mod](implicit classifier: Classifier[Mod, T]): Boolean =
      mods.exists(classifier.apply)
    def getAll[T <: Mod](implicit tag: ClassTag[T],
                         classifier: Classifier[Mod, T]): List[T] =
      mods.collect { case m if classifier.apply(m) => m.require[T] }
    def accessBoundary: Option[Name.Qualifier] = mods.flatMap(_.accessBoundary).headOption
    def getIncompatible[T <: Mod, U <: Mod]
      (implicit classifier1: Classifier[Mod, T], tag1: ClassTag[T],
                classifier2: Classifier[Mod, U], tag2: ClassTag[U]): List[(Mod, Mod)] =
      getAll[T].zip(getAll[U])
  }

  implicit class XtensionStat(stat: Stat) {
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

  implicit class XtensionCase(tree: Case) {
    def stats: Seq[Stat] = tree.body match {
      case Term.Block(stats) => stats
      case body => List(body)
    }
  }

  implicit class XtensionPatArg(tree: Pat.Arg) {
    // NOTE: see comments to Pat.Var.Term for explanation why this method is necessary
    def isIllegal: Boolean = tree match {
      case _: Pat.Var.Term.Quasi => false
      case Pat.Var.Term(Term.Name(value)) => value(0).isUpper
      case _ => false
    }
    def isLegal: Boolean = !isIllegal
    def firstNonPatParent: Option[Tree] = {
      tree.parent match {
        case Some(pat: Pat) => pat.firstNonPatParent
        case other => other.map(_.require[Tree])
      }
    }
  }

  implicit class XtensionApply(tree: Term.Apply) {
    def argsc: Int = 1 + (tree.fun match { case fun: Term.Apply => fun.argsc; case _ => 0 })
  }

  def tpeToPattpe(tpe: Type): Pat.Type = {
    def loop(tpe: Type): Pat.Type = tpe match {
      case tpe: Type.Name => tpe
      case tpe: Type.Select => tpe
      case Type.Project(qual, name) => Pat.Type.Project(loop(qual), name)
      case tpe: Type.Singleton => tpe
      case Type.Apply(tpe, args) => Pat.Type.Apply(loop(tpe), args.map(loop))
      case Type.ApplyInfix(lhs, op, rhs) => Pat.Type.ApplyInfix(loop(lhs), op, loop(rhs))
      case Type.Function(params, res) => Pat.Type.Function(params.map(param => loop(param.require[Type])), loop(res))
      case Type.Tuple(elements) => Pat.Type.Tuple(elements.map(loop))
      case Type.With(lhs, rhs) => Pat.Type.With(loop(lhs), loop(rhs))
      case Type.And(lhs, rhs) => Pat.Type.And(loop(lhs), loop(rhs))
      case Type.Or(lhs, rhs) => Pat.Type.Or(loop(lhs), loop(rhs))
      case Type.Refine(tpe, stats) => Pat.Type.Refine(tpe.map(loop), stats)
      case Type.Existential(tpe, stats) => Pat.Type.Existential(loop(tpe), stats)
      case Type.Annotate(tpe, annots) => Pat.Type.Annotate(loop(tpe), annots)
      case Type.Placeholder(bounds) => Pat.Type.Placeholder(bounds)
      case tpe: Lit => tpe
    }
    loop(tpe.require[Type])
  }

  def pattpeToTpe(pattpe: Pat.Type): Type = {
    def loop(tpe: Pat.Type): Type = tpe match {
      case tpe: Type.Name => tpe
      case tpe: Type.Select => tpe
      case tpe: Type.Singleton => tpe
      case tpe: Pat.Var.Type => ???
      case tpe: Pat.Type.Wildcard => Type.Placeholder(Type.Bounds(None, None))
      case Pat.Type.Project(qual, name) => Type.Project(loop(qual), name)
      case Pat.Type.Apply(tpe, args) => Type.Apply(loop(tpe), args.map(loop))
      case Pat.Type.ApplyInfix(lhs, op, rhs) => Type.ApplyInfix(loop(lhs), op, loop(rhs))
      case Pat.Type.Function(params, res) => Type.Function(params.map(loop), loop(res))
      case Pat.Type.Tuple(elements) => Type.Tuple(elements.map(loop))
      case Pat.Type.With(lhs, rhs) => Type.With(loop(lhs), loop(rhs))
      case Pat.Type.And(lhs, rhs) => Type.And(loop(lhs), loop(rhs))
      case Pat.Type.Or(lhs, rhs) => Type.Or(loop(lhs), loop(rhs))
      case Pat.Type.Refine(tpe, stats) => Type.Refine(tpe.map(loop), stats)
      case Pat.Type.Existential(tpe, stats) => Type.Existential(loop(tpe), stats)
      case Pat.Type.Annotate(tpe, annots) => Type.Annotate(loop(tpe), annots)
      case Pat.Type.Placeholder(bounds) => Type.Placeholder(bounds)
      case tpe: Lit => tpe
    }
    loop(pattpe.require[Pat.Type])
  }

  def tpeToCtorref(tpe: Type, ctor: Ctor.Name): Ctor.Call = {
    val tpe0 = tpe
    def loop(tpe: Type, ctor: Ctor.Name): Ctor.Call = {
      object Types {
        def unapply(tpes: Seq[Type.Arg]): Option[Seq[Type]] = {
          if (tpes.forall(_.is[Type])) Some(tpes.map(_.require[Type]))
          else None
        }
      }
      tpe match {
        case tpe @ Type.Name(value) =>
          ctor.copy(value = tpe.value)
        case tpe =>
          tpe match {
            case Type.Select(qual, tpe @ Type.Name(value)) => Ctor.Ref.Select(qual, ctor.copy(value = tpe.value))
            case Type.Project(qual, tpe @ Type.Name(value)) => Ctor.Ref.Project(qual, ctor.copy(value = tpe.value))
            case Type.Function(Types(params), ret) => Term.ApplyType(Ctor.Ref.Function(ctor), params :+ ret)
            case Type.Annotate(tpe, annots) => Term.Annotate(loop(tpe, ctor), annots)
            case Type.Apply(tpe, args) => Term.ApplyType(loop(tpe, ctor), args)
            case Type.ApplyInfix(lhs, op, rhs) => Term.ApplyType(loop(op, ctor), List(lhs, rhs))
            case _ => unreachable(debug(tpe0, tpe0.show[Structure], tpe, tpe.show[Structure]))
          }
      }
    }
    loop(tpe.require[Type], ctor.require[Ctor.Name])
  }

  def arrayClass(clazz: Class[_], rank: Int): Class[_] = {
    import scala.runtime.ScalaRunTime
    Predef.require(rank >= 0)
    if (rank == 0) clazz
    else arrayClass(ScalaRunTime.arrayClass(clazz), rank - 1)
  }

  object TermApply {
    def apply(fun: Term, argss: List[List[Term]]): Term = argss match {
      case args :: rest => rest.foldLeft(Term.Apply(fun, args)) { (acc, args) => Term.Apply(acc, args) }
      case _ => Term.Apply(fun, Nil)
    }

    def unapply(call: Term.Apply): Option[(Term, Seq[Seq[Term]])] = {
      def recur(acc: Seq[Seq[Term]], term: Term): (Term, Seq[Seq[Term]])  = term match {
        case Term.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

  implicit class XtensionTreeRoot(tree: Tree) {
    def root: Tree = tree.parent match {
      case Some(parent) => parent.root
      case None => tree
    }
  }
}
