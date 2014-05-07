package org.scalareflect.adt

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import org.scalareflect.invariants.nonEmpty
import scala.reflect.macros.whitebox.Context

class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.root
}

// TODO: withXXX and mapXXX for intermediate methods in branch traits (e.g. paramss/explicitss/implicits in Has.Paramss)
class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.branch
}

class leaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.leaf
}

class AdtMacros(val c: Context) {
  import c.universe._
  import definitions._
  import Flag._
  val Internal = q"_root_.org.scalareflect.adt.Internal"

  def root(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @root traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@root traits cannot be final")
      val flags1 = flags | SEALED
      val tag = q"private[core] def tag: _root_.scala.Int"
      val thisType = q"type ThisType <: ${cdef.name}"
      val hierarchyCheck = q"$Internal.hierarchyCheck[${cdef.name}]"
      val stats1 = tag +: thisType +: hierarchyCheck +: stats
      val anns1 = q"new $Internal.root" +: anns
      ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents, self, stats1))
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }

  def branch(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef): ClassDef = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @branch traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags | SEALED
      val thisType = q"override type ThisType <: ${cdef.name}"
      val hierarchyCheck = q"$Internal.hierarchyCheck[${cdef.name}]"
      val stats1 = thisType +: hierarchyCheck +: stats
      val anns1 = q"new $Internal.branch" +: anns
      ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents, self, stats1))
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @branch")
    }
    q"{ ..$expanded; () }"
  }

  def leaf(annottees: Tree*): Tree = {
    def transformLeafClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"${mods @ Modifiers(flags, privateWithin, anns)} class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @leaf classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @leaf classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @leaf classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@leaf classes cannot be abstract")
      val nonImplicitParamss = paramss.filter(_.forall(!_.mods.hasFlag(IMPLICIT)))
      if (nonImplicitParamss.length != 1) c.abort(cdef.pos, "@leaf classes must have exactly one non-implicit parameter list")
      val params = paramss.head
      params.foreach(p => if (p.mods.hasFlag(MUTABLE)) c.abort(p.pos, "@leaf classes must be immutable"))
      val flags1 = flags | CASE | FINAL
      def unprivateThis(mods: Modifiers) = {
        val Modifiers(flags, privateWithin, anns) = mods
        val flags1 = flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.LOCAL) & (~scala.reflect.internal.Flags.PRIVATE)
        Modifiers(flags1.asInstanceOf[FlagSet], privateWithin, anns)
      }
      val params1 = params.map{ case q"$mods val $name: $tpt = $default" => q"${unprivateThis(mods)} val $name: $tpt = $default" }
      val thisType = q"override type ThisType = ${cdef.name}"
      val tag = q"private[core] def tag: _root_.scala.Int = $Internal.calculateTag[${cdef.name}]"
      val withes = params.map{p =>
        val withName = TermName("with" + p.name.toString.capitalize)
        q"def $withName(${p.name}: ${p.tpt}): ThisType = this.copy(${p.name} = ${p.name})"
      }
      val maps = params.map{p =>
        val mapName = TermName("map" + p.name.toString.capitalize)
        q"def $mapName(f: ${p.tpt} => ${p.tpt}): ThisType = this.copy(${p.name} = f(this.${p.name}))"
      }
      val nullChecks = params.map(p => q"$Internal.nullCheck(this.${p.name})")
      val emptyChecks = params.map(p => q"$Internal.emptyCheck(this.${p.name})")
      val hierarchyCheck = q"$Internal.hierarchyCheck[${cdef.name}]"
      val immutabilityCheck = q"$Internal.immutabilityCheck[${cdef.name}]"
      val stats1 = (thisType +: tag +: hierarchyCheck +: immutabilityCheck +: stats) ++ withes ++ maps ++ nullChecks ++ emptyChecks
      val anns1 = q"new $Internal.leaf" +: anns
      val cdef1 = q"${Modifiers(flags1, privateWithin, anns1)} class $name[..$tparams] $ctorMods(..$params1) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val ModuleDef(mmods @ Modifiers(mflags, mprivateWithin, manns), mname, Template(mparents, mself, mstats)) = mdef
      val empties = if (params.nonEmpty && params.forall(_.rhs.nonEmpty)) List(q"val empty = $mname()(_root_.scala.reflect.core.SourceContext.None)") else Nil
      val mstats1 = (mstats ++ empties) :+ tag
      val manns1 = q"new $Internal.leaf" +: manns
      val mdef1 = ModuleDef(Modifiers(mflags, mprivateWithin, manns1), mname, Template(mparents, mself, mstats1))
      List(cdef1, mdef1)
    }
    def transformLeafModule(mdef: ModuleDef): ModuleDef = {
      val ModuleDef(mods @ Modifiers(flags, privateWithin, anns), name, Template(parents, self, stats)) = mdef
      val thisType = q"override type ThisType = $name.type"
      val tag = q"private[core] def tag: _root_.scala.Int = $Internal.calculateTag[$name.type]"
      val hierarchyCheck = q"$Internal.hierarchyCheck[$name.type]"
      val immutabilityCheck = q"$Internal.immutabilityCheck[$name.type]"
      val stats1 = stats :+ thisType :+ tag :+ hierarchyCheck :+ immutabilityCheck
      val anns1 = q"new $Internal.leaf" +: anns
      ModuleDef(Modifiers(flags, privateWithin, anns1), name, Template(parents, self, stats1))
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transformLeafClass(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transformLeafClass(cdef, q"object ${name.toTermName}") ++ rest
      case (mdef @ ModuleDef(_, _, _)) :: rest => transformLeafModule(mdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes can be @leaf")
    }
    q"{ ..$expanded; () }"
  }
}

object Internal {
  class root extends StaticAnnotation
  class branch extends StaticAnnotation
  class leaf extends StaticAnnotation
  case class TagAttachment(counter: Int)
  def calculateTag[T]: Int = macro AdtHelperMacros.calculateTag[T]
  def nullCheck[T](x: T): Unit = macro AdtHelperMacros.nullCheck
  def emptyCheck[T](x: T): Unit = macro AdtHelperMacros.emptyCheck
  def hierarchyCheck[T]: Unit = macro AdtHelperMacros.hierarchyCheck[T]
  def immutabilityCheck[T]: Unit = macro AdtHelperMacros.immutabilityCheck[T]
}

class AdtHelperMacros(val c: Context) {
  import c.universe._
  import definitions._
  import c.internal._
  import decorators._
  import Internal.TagAttachment

  implicit class RichSymbol(sym: Symbol) {
    def nonEmpty = {
      val tptAnns = sym.info match {
        case AnnotatedType(anns, _) => anns
        case _ => Nil
      }
      def hasNonEmpty(anns: List[Annotation]) = anns.exists(_.tree.tpe =:= typeOf[nonEmpty])
      hasNonEmpty(sym.annotations) || hasNonEmpty(tptAnns)
    }
    def isRoot = sym.annotations.exists(_.tree.tpe =:= typeOf[Internal.root])
    def isBranch = sym.annotations.exists(_.tree.tpe =:= typeOf[Internal.branch])
    def isLeaf = sym.annotations.exists(_.tree.tpe =:= typeOf[Internal.leaf])
    def root = sym.asClass.baseClasses.reverse.find(_.isRoot).getOrElse(NoSymbol)
  }

  def calculateTag[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val sym = T.tpe.typeSymbol.asClass
    val tag = sym.attachments.get[TagAttachment].map(_.counter).getOrElse {
      val att = sym.root.attachments.get[TagAttachment].map(att => att.copy(counter = att.counter + 1)).getOrElse(new TagAttachment(1))
      sym.root.updateAttachment(att)
      sym.updateAttachment(att)
      att.counter
    }
    q"$tag"
  }

  def nullCheck(x: c.Tree): c.Tree = {
    if (x.tpe.baseClasses.contains(ObjectClass)) q"_root_.org.scalareflect.invariants.require($x != null)"
    else q""
  }

  def emptyCheck(x: c.Tree): c.Tree = {
    if (x.symbol.asTerm.accessed.nonEmpty) q"_root_.org.scalareflect.invariants.require($x.nonEmpty)"
    else q""
  }

  def hierarchyCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val sym = T.tpe.typeSymbol.asClass
    val designation = if (sym.isRoot) "root" else if (sym.isBranch) "branch" else if (sym.isLeaf) "leaf" else ???
    val roots = sym.baseClasses.filter(_.isRoot)
    if (roots.length == 0) c.abort(c.enclosingPosition, s"rootless $designation is disallowed")
    else if (roots.length > 1) c.abort(c.enclosingPosition, s"multiple roots for a $designation: " + (roots.map(_.fullName).init.mkString(", ")) + " and " + roots.last.fullName)
    val root = roots.head
    sym.baseClasses.map(_.asClass).foreach{bsym =>
      val exempt =
        bsym.isModuleClass ||
        bsym == ObjectClass ||
        bsym == AnyClass ||
        bsym == symbolOf[scala.Serializable] ||
        bsym == symbolOf[java.io.Serializable] ||
        bsym == symbolOf[scala.Product] ||
        bsym == symbolOf[scala.Equals] ||
        root.info.baseClasses.contains(bsym)
      if (!exempt && !bsym.isRoot && !bsym.isBranch && !bsym.isLeaf) c.abort(c.enclosingPosition, s"outsider parent of a $designation: ${bsym.fullName}")
      if (!exempt && !bsym.isSealed && !bsym.isFinal) c.abort(c.enclosingPosition, s"unsealed parent of a $designation: ${bsym.fullName}")
    }
    q""
  }

  def immutabilityCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    def check(sym: Symbol): Unit = {
      val vars = sym.info.members.collect { case x: TermSymbol if !x.isMethod && x.isVar => x }
      vars.foreach(v => c.abort(c.enclosingPosition, "leafs can't have mutable state: " + v.owner.fullName + "." + v.name))
      val vals = sym.info.members.collect { case x: TermSymbol if !x.isMethod && !x.isVar => x }
      vals.foreach(v => ()) // TODO: deep immutability check
    }
    check(T.tpe.typeSymbol.asClass)
    q""
  }
}