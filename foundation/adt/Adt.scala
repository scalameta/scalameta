package org.scalareflect.adt

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import org.scalareflect.invariants.nonEmpty
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

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
      val tag = q"private[reflect] def tag: _root_.scala.Int"
      val thisType = if (stats.collect{ case TypeDef(_, TypeName("ThisType"), _, _) => () }.isEmpty) q"type ThisType <: ${cdef.name}" else q""
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
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val parents1 = ListBuffer[Tree]() ++ parents
      val stats1 = ListBuffer[Tree]() ++ stats
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      def unprivate(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.LOCAL) & (~scala.reflect.internal.Flags.PRIVATE)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def casify(mods: Modifiers) = Modifiers(mods.flags | CASE, mods.privateWithin, mods.annotations)
      def finalize(mods: Modifiers) = Modifiers(mods.flags | FINAL, mods.privateWithin, mods.annotations)

      // step 1: validate the shape of the class
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @leaf classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @leaf classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @leaf classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@leaf classes cannot be abstract")
      if (ctorMods.flags != NoFlags) c.abort(cdef.pos, "@leaf classes must define a public primary constructor")
      if (paramss.length != 1) c.abort(cdef.pos, "@leaf classes must define exactly one parameter list")

      // step 2: unprivate parameters, create copy-on-write helpers, generate validation checks
      val params :: Nil = paramss
      val params1 = params.map{ case q"$mods val $name: $tpt = $default" => q"${unprivate(mods)} val $name: $tpt = $default" }
      stats1 ++= params.map{p =>
        val withName = TermName("with" + p.name.toString.capitalize)
        q"def $withName(${p.name}: ${p.tpt}): ThisType = this.copy(${p.name} = ${p.name})"
      }
      stats1 ++= params.map{p =>
        val mapName = TermName("map" + p.name.toString.capitalize)
        q"def $mapName(f: ${p.tpt} => ${p.tpt}): ThisType = this.copy(${p.name} = f(this.${p.name}))"
      }
      stats1 ++= params.map(p => q"$Internal.nullCheck(this.${p.name})")
      stats1 ++= params.map(p => q"$Internal.emptyCheck(this.${p.name})")

      // step 3: generate boilerplate required by the @adt infrastructure
      stats1 += q"override type ThisType = $name"
      stats1 += q"private[reflect] def tag: _root_.scala.Int = $Internal.calculateTag[ThisType]"
      stats1 += q"$Internal.hierarchyCheck[ThisType]"
      stats1 += q"$Internal.immutabilityCheck[ThisType]"
      anns1 += q"new $Internal.leaf"
      manns1 += q"new $Internal.leaf"
      parents1 += tq"_root_.scala.Product"

      val cdef1 = q"${casify(finalize(mods1))} class $name[..$tparams] $ctorMods(..$params1) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }"
      List(cdef1, mdef1)
    }

    def transformLeafModule(mdef: ModuleDef): ModuleDef = {
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mparents1 = ListBuffer[Tree]() ++ mparents
      val mstats1 = ListBuffer[Tree]() ++ mstats
      def casify(mods: Modifiers) = Modifiers(mods.flags | CASE, mods.privateWithin, mods.annotations)

      // step 1: validate the shape of the module
      if (mmods.hasFlag(FINAL)) c.abort(mdef.pos, "final is redundant for @leaf classes")
      if (mmods.hasFlag(CASE)) c.abort(mdef.pos, "case is redundant for @leaf classes")

      // step 2: generate boilerplate required by the @adt infrastructure
      mstats1 += q"override type ThisType = $mname.type"
      mstats1 += q"private[reflect] def tag: _root_.scala.Int = $Internal.calculateTag[ThisType]"
      mstats1 += q"$Internal.hierarchyCheck[ThisType]"
      mstats1 += q"$Internal.immutabilityCheck[ThisType]"
      manns1 += q"new $Internal.leaf"
      mparents1 += tq"_root_.scala.Product"

      q"${casify(mmods1)} object $mname extends { ..$mearlydefns } with ..$mparents1 { $mself => ..$mstats1 }"
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
    val emptyCheckRequested =
      try x.symbol.asTerm.accessed.nonEmpty
      catch { case _: AssertionError => x.symbol.nonEmpty }
    if (emptyCheckRequested) q"_root_.org.scalareflect.invariants.require($x.nonEmpty)"
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
