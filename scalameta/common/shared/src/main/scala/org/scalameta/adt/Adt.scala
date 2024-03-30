package org.scalameta.adt

import org.scalameta.adt.{Metadata => AdtMetadata}
import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.MacroHelpers
import scala.meta.common.Optional

import scala.annotation.StaticAnnotation
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

// The @root, @branch and @leaf macros implement the ADT pattern in a formulation that we found useful.
//
// More precisely a single @root trait, multiple @branch traits and a bunch of @leaf classes
// define an ADT hierarchy. Unlike the traditional understanding of ADTs with a sealed root trait
// and several final classes, we introduce intermediate branch traits, all of which inherit from the root
// (or each other) and are ultimately inherited by one or more final leaf classes.
//
// Much as with the @data annotation, we use macro annotations to codify a well-known pattern
// and tailor it to our needs with the extensibility that macro annotations provide.

class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtNamerMacros.root
}

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtNamerMacros.branch
}

class leaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtNamerMacros.leaf
}

class none extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtNamerMacros.none
}

class AdtNamerMacros(val c: Context) extends MacroHelpers {
  import c.universe.Flag._
  import c.universe._

  def root(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(
        mods @ Modifiers(flags, privateWithin, anns),
        name,
        tparams,
        Template(parents, self, stats)
      ) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val classRef = typeRef(cdef, requireHk = false, requireWildcards = true)
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @root traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@root traits cannot be final")
      val flags1 = flags | SEALED
      mstats1 += q"$AdtTyperMacrosModule.hierarchyCheck[$classRef]"
      val anns1 = anns :+ q"new $AdtMetadataModule.root"
      val parents1 = parents :+ tq"$AdtMetadataModule.Adt" :+ tq"_root_.scala.Product" :+
        tq"_root_.scala.Serializable"

      val cdef1 = ClassDef(
        Modifiers(flags1, privateWithin, anns1),
        name,
        tparams,
        Template(parents1, self, stats1.toList)
      )
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
  })

  def branch(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] =
      transformImpl(cdef, mdef)

    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] =
      transformImpl(cdef, mdef)

    private def transformImpl(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(
        mods @ Modifiers(flags, privateWithin, anns),
        name,
        tparams,
        Template(parents, self, stats)
      ) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val classRef = typeRef(cdef, requireHk = false, requireWildcards = true)
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @branch traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags | SEALED
      mstats1 += q"$AdtTyperMacrosModule.hierarchyCheck[$classRef]"
      val anns1 = anns :+ q"new $AdtMetadataModule.branch"

      val cdef1 = ClassDef(
        Modifiers(flags1, privateWithin, anns1),
        name,
        tparams,
        Template(parents, self, stats1.toList)
      )
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
  })

  def leaf(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
        cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
        mdef
      val classRef = typeRef(cdef, requireHk = false, requireWildcards = true)
      val parents1 = ListBuffer[Tree]() ++ parents
      val stats1 = ListBuffer[Tree]() ++ stats
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mstats1 = ListBuffer[Tree]() ++ mstats

      // step 1: generate boilerplate required by the @adt infrastructure
      mstats1 += q"$AdtTyperMacrosModule.hierarchyCheck[$classRef]"
      mstats1 += q"$AdtTyperMacrosModule.immutabilityCheck[$classRef]"
      // mstats1 += q"$AdtTyperMacrosModule.consistencyCheck[$mname.type]"
      anns1 += q"new $AdtMetadataModule.leafClass"
      manns1 += q"new $AdtMetadataModule.leafCompanion"
      parents1 += tq"_root_.scala.Product"

      // step 2: do everything else via @data
      anns1 += q"new $DataAnnotation(...$argss)"

      val cdef1 =
        q"$mods1 class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 =
        q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }

    override def transformModule(mdef: ModuleDef): ModuleDef = {
      val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
        mdef
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mparents1 = ListBuffer[Tree]() ++ mparents
      val mstats1 = ListBuffer[Tree]() ++ mstats

      // step 1: generate boilerplate required by the @adt infrastructure
      mstats1 += q"$AdtTyperMacrosModule.hierarchyCheck[$mname.type]"
      mstats1 += q"$AdtTyperMacrosModule.immutabilityCheck[$mname.type]"
      // mstats1 += q"$AdtTyperMacrosModule.consistencyCheck[$mname.type]"
      manns1 += q"new $AdtMetadataModule.leafClass"
      mparents1 += tq"_root_.scala.Product"

      // step 2: do everything else via @data
      manns1 += q"new $DataAnnotation(...$argss)"

      q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents1 { $mself => ..$mstats1 }"
    }
  })

  def none(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformModule(mdef: ModuleDef): ModuleDef = {
      val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
        mdef
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mstats1 = ListBuffer[Tree]() ++ mstats

      manns1 += q"new $AdtPackage.leaf"
      manns1 += q"new $AdtMetadataModule.noneClass"
      mstats1 += q"override def isEmpty: $BooleanClass = true"

      q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
    }
  })
}

// Parts of @root, @branch and @leaf logic that need a typer context and can't be run in a macro annotation.
object AdtTyperMacros {
  def hierarchyCheck[T]: Unit = macro AdtTyperMacrosBundle.hierarchyCheck[T]
  def immutabilityCheck[T]: Unit = macro AdtTyperMacrosBundle.immutabilityCheck[T]
  def consistencyCheck[T]: Unit = macro AdtTyperMacrosBundle.consistencyCheck[T]
}

// NOTE: can't call this `AdtTyperMacros`, because then typechecking the macro defs will produce spurious cyclic errors
class AdtTyperMacrosBundle(val c: Context) extends AdtReflection with MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror

  import c.internal._
  import c.internal.decorators._
  import c.universe._

  private def fail(message: String): Nothing = c.abort(c.enclosingPosition, message)

  def hierarchyCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    checkHierarchy(T.tpe, fail, checkSealed = true)
    q"()"
  }

  def immutabilityCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    def check(sym: Symbol): Unit = {
      var vars = sym.info.members.collect { case x: TermSymbol if !x.isMethod && x.isVar => x }
      vars = vars.filter(!_.hasAnnotation[AdtMetadata.byNeedField])
      vars.foreach(v => fail("leafs can't have mutable state: " + v.owner.fullName + "." + v.name))
      val vals = sym.info.members.collect { case x: TermSymbol if !x.isMethod && !x.isVar => x }
      vals.foreach(v => ())
    }
    check(T.tpe.typeSymbol.asClass)
    q"()"
  }

  def optionalityCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    if (!(T.tpe <:< typeOf[Optional]))
      fail(s"@none objects must be in an ADT family that inherits from Optional")
    q"()"
  }

  def consistencyCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    lazy val sym = T.tpe.typeSymbol
    lazy val root = sym.asLeaf.root

    case class FailOnceAttachment()
    implicit class XtensionOptional(sym: Symbol) {
      def isNone = sym.hasAnnotation[AdtMetadata.noneClass]
    }
    def failOnce(message: String): Unit = if (!root.sym.attachments.contains[FailOnceAttachment]) {
      root.sym.attachments.update(FailOnceAttachment())
      fail(message)
    }

    if (sym.asType.toType <:< typeOf[Optional]) {
      val nones = root.allLeafs.map(_.sym).filter(_.isNone)
      if (nones.isEmpty)
        failOnce("an ADT family that inherits from Optional must define a @none object")
      else if (nones.length > 1)
        failOnce("an ADT family that inherits from Optional can't have multiple @none objects")
    } else if (sym.isNone)
      fail(s"@none objects must be in an ADT family that inherits from Optional")

    q"()"
  }
}
