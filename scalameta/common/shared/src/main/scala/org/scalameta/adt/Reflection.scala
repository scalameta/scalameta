package org.scalameta.adt

import org.scalameta.adt.{Metadata => AdtMetadata}
import scala.meta.internal.trees.{Metadata => AstMetadata}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.api.Universe
import scala.reflect.classTag

trait Reflection {
  val u: Universe
  val mirror: u.Mirror

  import u._
  import u.internal._
  import u.internal.decorators._

  implicit class XtensionAnnotatedSymbol(sym: Symbol) {
    // NOTE: Can't use TypeTag here, because this method can be called at runtime as well,
    // and at runtime we may run into classloader problems (I did, actually).
    def hasAnnotation[T: ClassTag]: Boolean = getAnnotation[T].nonEmpty
    def getAnnotation[T: ClassTag]: Option[Tree] = {
      sym.initialize
      val ann = sym.annotations
        .find(_.tree.tpe.typeSymbol.fullName == classTag[T].runtimeClass.getCanonicalName)
      ann.map(_.tree)
    }
  }

  implicit class XtensionAdtSymbol(sym: Symbol) {
    def isAdt: Boolean = {
      def inheritsFromAdt = sym.isClass && (sym.asClass.toType <:< typeOf[AdtMetadata.Adt])
      def isBookkeeping = sym.asClass == symbolOf[AdtMetadata.Adt] ||
        sym.asClass == symbolOf[AstMetadata.Ast]
      inheritsFromAdt && !isBookkeeping
    }
    def isRoot: Boolean = sym.hasAnnotation[AdtMetadata.root]
    def isBranch: Boolean = sym.hasAnnotation[AdtMetadata.branch]
    def isLeaf: Boolean = sym.hasAnnotation[AdtMetadata.leafClass]
    def isField: Boolean = isField(isPrivateOK = false)
    def isField(isPrivateOK: Boolean): Boolean = sym match {
      case m: MethodSymbolApi if m.owner.isLeaf =>
        sym.hasAnnotation[AstMetadata.astField] || isPrivateOK && isPrivateField ||
        m.isPublic && m.isParamAccessor && m.isGetter
      case _ => false
    }
    private[adt] def isAstClass: Boolean = sym.hasAnnotation[AstMetadata.astClass]
    private[adt] def isAuxiliaryField: Boolean = sym.hasAnnotation[AstMetadata.auxiliary]
    private[adt] def isPrivateField: Boolean = sym.hasAnnotation[AdtMetadata.privateField]
    private[adt] def isPayloadField(isPrivateOK: Boolean): Boolean = !isAuxiliaryField &&
      (isPrivateOK || !isPrivateField)
    def asAdt: Adt =
      if (isRoot) sym.asRoot
      else if (isBranch) sym.asBranch
      else if (isLeaf) sym.asLeaf
      else sys.error("not an adt: " + sym)
    def asRoot: Root = new Root(sym)
    def asBranch: Branch = new Branch(sym)
    def asLeaf: Leaf = new Leaf(sym)
    def asField: Field = new Field(sym)
  }

  protected def figureOutDirectSubclasses(sym: ClassSymbol): List[Symbol] =
    if (sym.isSealed) sym.knownDirectSubclasses.toList.sortBy(_.fullName)
    else sys.error(s"failed to figure out direct subclasses for ${sym.fullName}")

  private implicit class PrivateXtensionAdtSymbol(sym: Symbol) {
    private def ensureModule(sym: Symbol): Symbol =
      if (sym.isModuleClass) sym.owner.info.member(sym.name.toTermName) else sym
    def branches: List[Symbol] = {
      sym.initialize; figureOutDirectSubclasses(sym.asClass).filter(_.isBranch)
    }
    def allBranches: List[Symbol] = (sym.branches ++ sym.branches.flatMap(_.allBranches)).distinct
    def leafs: List[Symbol] = {
      sym.initialize
      figureOutDirectSubclasses(sym.asClass).filter(_.isLeaf).map(ensureModule)
    }
    def allLeafs: List[Symbol] = (sym.leafs ++ sym.branches.flatMap(_.allLeafs)).map(ensureModule)
      .distinct

    def root: Symbol = sym.asClass.baseClasses.reverse.find(_.isRoot).getOrElse(NoSymbol)
    def allFields: List[Symbol] = {
      val isPrivateOK = sym.isLeaf
      sym.info.decls.sorted.filter(_.isField(isPrivateOK))
    }
  }

  abstract class Adt(val sym: Symbol) {
    def tpe: Type = if (sym.isTerm) sym.info else sym.asType.toType
    def prefixes: List[String] = {
      @tailrec
      def loop(sym: Symbol, suffixes: List[String]): List[String] = {
        // if owner is a package or a package object, it shouldn't be part of the prefix
        val owner = sym.owner
        val names = sym.name.toString :: suffixes
        if (owner.isPackageClass || owner.name == typeNames.PACKAGE) names else loop(owner, names)
      }
      loop(sym, Nil)
    }
    def prefix: String = prefixes.mkString(".")
    def root = sym.root.asRoot
    def parents = sym.asClass.baseClasses.filter(sym1 => sym1 != sym && sym1.isAdt).map(_.asAdt)
    def <:<(other: Adt) = sym.asClass.toType <:< other.sym.asClass.toType
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
    def fields: List[Field] = fields(false)
    def fields(isPrivateOK: Boolean): List[Field] = allFields
      .filter(_.sym.isPayloadField(isPrivateOK))
    def allFields: List[Field] = sym.allFields.map(_.asField)
    override def toString = s"leaf $prefix"
  }
  class Field(val sym: Symbol) {
    if (!sym.isField(isPrivateOK = true)) sys.error(s"$sym is not a field")
    def owner: Leaf = sym.owner.asLeaf
    def name: TermName = TermName(sym.name.toString.stripPrefix("_"))
    def tpe: Type = sym.info.finalResultType
    override def toString = s"field ${owner.prefix}.$name: $tpe" +
      (if (sym.isAuxiliaryField) " (auxiliary)" else "")
  }

  private def isExemptParentSymbol(bsym: ClassSymbol): Boolean = bsym.isModuleClass ||
    bsym == symbolOf[Object] || bsym == symbolOf[Any] || bsym == symbolOf[scala.Serializable] ||
    bsym == symbolOf[java.io.Serializable] || bsym == symbolOf[scala.Product] ||
    bsym == symbolOf[scala.Equals]

  protected def checkHierarchy(tpe: Type, fail: String => Unit, checkSealed: Boolean): Unit = {
    val sym = tpe.typeSymbol.asClass
    def designation =
      if (sym.isRoot) "root" else if (sym.isBranch) "branch" else if (sym.isLeaf) "leaf" else ???
    val roots = sym.baseClasses.filter(_.isRoot)
    if (roots.isEmpty && sym.isLeaf) fail(s"rootless leaf is disallowed")
    else if (roots.length > 1) fail(
      s"multiple roots for a $designation: " + (roots.map(_.fullName).init.mkString(", ")) +
        " and " + roots.last.fullName
    )
    val root = roots.headOption.getOrElse(NoSymbol)
    sym.baseClasses.map(_.asClass).foreach { bsym =>
      val exempt = isExemptParentSymbol(bsym) || root.info.baseClasses.contains(bsym)
      if (!exempt && !bsym.isRoot && !bsym.isBranch && !bsym.isLeaf)
        fail(s"outsider parent of a $designation: ${bsym.fullName}")
      if (checkSealed && !exempt && !bsym.isSealed && !bsym.isFinal)
        fail(s"unsealed parent of a $designation: ${bsym.fullName}")
    }
  }

}
