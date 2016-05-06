package org.scalameta.adt

import scala.reflect.api.Universe
import org.scalameta.adt.{Metadata => AdtMetadata}
import scala.meta.internal.ast.{Metadata => AstMetadata}
import scala.reflect.{classTag, ClassTag}
import scala.collection.mutable

trait Reflection {
  val u: Universe
  val mirror: u.Mirror
  import u._
  import internal._
  import decorators._

  implicit class XtensionAnnotatedSymbol(sym: Symbol) {
    // NOTE: Can't use TypeTag here, because this method can be called at runtime as well,
    // and at runtime we may run into classloader problems (I did, actually).
    def hasAnnotation[T: ClassTag]: Boolean = getAnnotation[T].nonEmpty
    def getAnnotation[T: ClassTag]: Option[Tree] = {
      sym.initialize
      val ann = sym.annotations.find(_.tree.tpe.typeSymbol.fullName == classTag[T].runtimeClass.getCanonicalName)
      ann.map(_.tree)
    }
  }

  implicit class XtensionAdtSymbol(sym: Symbol) {
    def isAdt: Boolean = {
      def inheritsFromAdt = sym.isClass && (sym.asClass.toType <:< typeOf[AdtMetadata.Adt])
      def isBookkeeping = sym.asClass == symbolOf[AdtMetadata.Adt] || sym.asClass == symbolOf[AstMetadata.Ast]
      inheritsFromAdt && !isBookkeeping
    }
    def isRoot: Boolean = sym.hasAnnotation[AdtMetadata.root]
    def isBranch: Boolean = sym.hasAnnotation[AdtMetadata.branch]
    def isLeaf: Boolean = sym.hasAnnotation[AdtMetadata.leafClass]
    def isField: Boolean = {
      val isMethodInLeafClass = sym.isMethod && sym.owner.isLeaf
      val isParamGetter = sym.isTerm && sym.asTerm.isParamAccessor && sym.asTerm.isGetter && sym.isPublic
      val isAstField = sym.hasAnnotation[AstMetadata.astField]
      isMethodInLeafClass && (isParamGetter || isAstField)
    }
    def isPayload: Boolean = sym.isField && !sym.isAuxiliary
    def isAuxiliary: Boolean = sym.isField && sym.hasAnnotation[AstMetadata.auxiliary]
    def isByNeed: Boolean = sym.isField && sym.hasAnnotation[AdtMetadata.byNeedField]
    def asAdt: Adt = if (isRoot) sym.asRoot else if (isBranch) sym.asBranch else if (isLeaf) sym.asLeaf else sys.error("not an adt: " + sym)
    def asRoot: Root = new Root(sym)
    def asBranch: Branch = new Branch(sym)
    def asLeaf: Leaf = new Leaf(sym)
    def asField: Field = new Field(sym)
  }

  protected def figureOutDirectSubclasses(sym: ClassSymbol): List[Symbol] = {
    if (sym.isSealed) sym.knownDirectSubclasses.toList.sortBy(_.fullName)
    else sys.error(s"failed to figure out direct subclasses for ${sym.fullName}")
  }

  private implicit class PrivateXtensionAdtSymbol(sym: Symbol) {
    private def ensureModule(sym: Symbol): Symbol = if (sym.isModuleClass) sym.owner.info.member(sym.name.toTermName) else sym
    def branches: List[Symbol] = { sym.initialize; figureOutDirectSubclasses(sym.asClass).toList.filter(_.isBranch) }
    def allBranches: List[Symbol] = (sym.branches ++ sym.branches.flatMap(_.allBranches)).distinct
    def leafs: List[Symbol] = { sym.initialize; figureOutDirectSubclasses(sym.asClass).toList.filter(_.isLeaf).map(ensureModule) }
    def allLeafs: List[Symbol] = (sym.leafs ++ sym.branches.flatMap(_.allLeafs)).map(ensureModule).distinct

    def root: Symbol = sym.asClass.baseClasses.reverse.find(_.isRoot).getOrElse(NoSymbol)
    def fields: List[Symbol] = allFields.filter(p => p.isPayload)
    def allFields: List[Symbol] = sym.info.decls.filter(_.isField).toList
  }

  abstract class Adt(val sym: Symbol) {
    def tpe: Type = if (sym.isTerm) sym.info else sym.asType.toType
    def prefix: String = {
      def loop(sym: Symbol): String = {
        // if owner is a package or a package object, it shouldn't be part of the prefix
        if (sym.owner.isPackageClass || sym.owner.name == typeNames.PACKAGE) sym.name.toString
        else loop(sym.owner) + "." + sym.name.toString
      }
      loop(sym)
    }
    def root = sym.root.asRoot
    def parents = sym.asClass.baseClasses.filter(sym1 => sym1 != sym && sym1.isAdt).map(_.asAdt)
    def <:< (other: Adt) = sym.asClass.toType <:< other.sym.asClass.toType
    override def equals(that: Any) = that match {
      case that: Adt => this.sym == that.sym
      case _ => false
    }
    override def hashCode = sym.hashCode
  }
  trait NonLeafApi extends Adt {
    def all: List[Adt] = List(this) ++ this.allBranches ++ this.allLeafs
    def branches: List[Branch] = sym.branches.map(_.asBranch)
    def allBranches: List[Branch] = sym.allBranches.map(_.asBranch)
    def leafs: List[Leaf] = sym.leafs.map(_.asLeaf)
    def allLeafs: List[Leaf] = sym.allLeafs.map(_.asLeaf)
  }
  class Root(sym: Symbol) extends Adt(sym) with NonLeafApi {
    if (!sym.isRoot) sys.error(s"$sym is not a root")
    override def toString = s"root $prefix"
  }
  class Branch(sym: Symbol) extends Adt(sym) with NonLeafApi {
    if (!sym.isBranch) sys.error(s"$sym is not a branch")
    override def toString = s"branch $prefix"
  }
  class Leaf(sym: Symbol) extends Adt(sym) {
    if (!sym.isLeaf) sys.error(s"$sym is not a leaf")
    def fields: List[Field] = sym.fields.map(_.asField)
    def allFields: List[Field] = sym.allFields.map(_.asField)
    override def toString = s"leaf $prefix"
  }
  class Field(val sym: Symbol) {
    if (!sym.isField) sys.error(s"$sym is not a field")
    def owner: Leaf = sym.owner.asLeaf
    def name: TermName = TermName(sym.name.toString.stripPrefix("_"))
    def tpe: Type = sym.info.finalResultType
    def isPayload: Boolean = sym.isPayload
    def isAuxiliary: Boolean = sym.isAuxiliary
    def isByNeed: Boolean = sym.isByNeed
    override def toString = s"field ${owner.prefix}.$name: $tpe" + (if (isAuxiliary) " (auxiliary)" else "")
  }
}
