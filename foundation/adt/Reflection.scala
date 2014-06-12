package org.scalameta.adt

import scala.reflect.api.Universe
import org.scalameta.adt.{Internal => AdtInternal}
import org.scalameta.{ast => AstInternal}
import scala.reflect.{classTag, ClassTag}

trait AdtReflection {
  val u: Universe
  import u._
  import internal._
  import decorators._

  implicit class AdtSymbolOps(val sym: Symbol) {
    def isAdt: Boolean = sym.isClass && (sym.asClass.toType <:< typeOf[AdtInternal.Adt])
    private def hasAnnotation[T: ClassTag] = sym.annotations.exists(_.tree.tpe.typeSymbol.fullName == classTag[T].runtimeClass.getCanonicalName)
    def isRoot: Boolean = hasAnnotation[AdtInternal.root]
    def isBranch: Boolean = hasAnnotation[AdtInternal.branch]
    def isLeaf: Boolean = hasAnnotation[AdtInternal.leafClass]
    def isPayload: Boolean = sym.isTerm && sym.isParameter && !sym.isManualTrivia && !sym.isAutoTrivia
    def isManualTrivia: Boolean = hasAnnotation[AstInternal.trivia] && !hasAnnotation[AstInternal.auto]
    def isAutoTrivia: Boolean = hasAnnotation[AstInternal.trivia] && hasAnnotation[AstInternal.auto]
    def asAdt: Adt = if (isRoot) sym.asRoot else if (isBranch) sym.asBranch else if (isLeaf) sym.asLeaf else sys.error("not an adt")
    def asRoot: Root = new Root(sym)
    def asBranch: Branch = new Branch(sym)
    def asLeaf: Leaf = new Leaf(sym)
    def asField: Field = new Field(sym)
  }

  private implicit class PrivateAdtSymbolOps(val sym: Symbol) {
    private def ensureModule(sym: Symbol): Symbol = if (sym.isModuleClass) sym.owner.info.member(sym.name.toTermName) else sym
    def branches: List[Symbol] = { sym.initialize; sym.asClass.knownDirectSubclasses.toList.filter(_.isBranch) }
    def allBranches: List[Symbol] = sym.branches ++ sym.branches.flatMap(_.allBranches).distinct
    def leafs: List[Symbol] = { sym.initialize; sym.asClass.knownDirectSubclasses.toList.filter(_.isLeaf).map(ensureModule) }
    def allLeafs: List[Symbol] = sym.leafs ++ sym.branches.flatMap(_.allLeafs).distinct.map(ensureModule)

    def root: Symbol = sym.asClass.baseClasses.reverse.find(_.isRoot).getOrElse(NoSymbol)
    private def secondParamList: List[Symbol] = sym.info.decls.collect{ case ctor: MethodSymbol if ctor.isPrimaryConstructor => ctor }.head.paramLists(1)
    def fields: List[Symbol] = secondParamList.filter(p => p.isPayload || p.isManualTrivia)
    def nontriviaFields: List[Symbol] = secondParamList.filter(p => p.isPayload)
    def allFields: List[Symbol] = secondParamList
  }

  trait CommonApi {
    def sym: Symbol
    def prefix: String = {
      def loop(sym: Symbol): String = {
        if (sym.owner.isPackageClass) sym.name.toString
        else loop(sym.owner) + "." + sym.name.toString
      }
      loop(sym)
    }
    def root = sym.root.asRoot
  }
  abstract class Adt(val sym: Symbol) extends CommonApi
  trait NonLeafApi extends CommonApi {
    def branches: List[Branch] = sym.branches.map(_.asBranch)
    def allBranches: List[Branch] = sym.allBranches.map(_.asBranch)
    def leafs: List[Leaf] = sym.leafs.map(_.asLeaf)
    def allLeafs: List[Leaf] = sym.allLeafs.map(_.asLeaf)
  }
  class Root(sym: Symbol) extends Adt(sym) with NonLeafApi {
    require(sym.isRoot)
    override def toString = s"root $prefix"
  }
  class Branch(sym: Symbol) extends Adt(sym) with NonLeafApi {
    require(sym.isBranch)
    override def toString = s"branch $prefix"
  }
  class Leaf(sym: Symbol) extends Adt(sym) {
    require(sym.isLeaf)
    def fields: List[Field] = sym.fields.map(_.asField)
    def nontriviaFields: List[Field] = sym.nontriviaFields.map(_.asField)
    def allFields: List[Field] = sym.allFields.map(_.asField)
    override def toString = s"leaf $prefix"
  }
  class Field(val sym: Symbol) {
    require(sym.isTerm && sym.isParameter)
    def name: TermName = TermName(sym.name.toString.stripPrefix("_"))
    def tpe: Type = sym.info
    def isManualTrivia: Boolean = sym.isManualTrivia
    def isAutoTrivia: Boolean = sym.isAutoTrivia
    override def toString = s"field $name: $tpe" + (if (isManualTrivia) " (manual trivia)" else if (isAutoTrivia) " (auto trivia)" else "")
  }
}
