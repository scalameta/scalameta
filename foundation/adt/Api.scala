package org.scalareflect.adt

import scala.reflect.macros.blackbox.Context
import org.scalareflect.adt.{Internal => AdtInternal}
import org.scalareflect.{ast => AstInternal}

trait AdtReflection {
  val c: Context
  import c.universe._
  import c.internal._
  import decorators._

  implicit class AdtSymbolOps(val sym: Symbol) {
    private def hasAnnotation[T: TypeTag] = sym.annotations.exists(_.tree.tpe.typeSymbol == typeOf[T].typeSymbol)
    def isRoot: Boolean = hasAnnotation[AdtInternal.root]
    def isBranch: Boolean = hasAnnotation[AdtInternal.branch]
    def isLeaf: Boolean = hasAnnotation[AdtInternal.leafClass]
    def isPayload: Boolean = sym.isTerm && sym.isParameter && !sym.isManualTrivia && !sym.isAutoTrivia
    def isManualTrivia: Boolean = hasAnnotation[AstInternal.trivia] && !hasAnnotation[AstInternal.auto]
    def isAutoTrivia: Boolean = hasAnnotation[AstInternal.trivia] && hasAnnotation[AstInternal.auto]

    private def ensureModule(sym: Symbol): Symbol = if (sym.isModuleClass) sym.owner.info.member(sym.name.toTermName) else sym
    def branches: List[Symbol] = { sym.initialize; sym.asClass.knownDirectSubclasses.toList.filter(_.isBranch) }
    def allBranches: List[Symbol] = sym.branches ++ sym.branches.flatMap(_.allBranches)
    def leafs: List[Symbol] = { sym.initialize; sym.asClass.knownDirectSubclasses.toList.filter(_.isLeaf).map(ensureModule) }
    def allLeafs: List[Symbol] = sym.leafs ++ sym.leafs.flatMap(_.allBranches).map(ensureModule)

    private def secondParamList: List[Symbol] = sym.info.decls.collect{ case ctor: MethodSymbol if ctor.isPrimaryConstructor => ctor }.head.paramLists(1)
    def fields: List[Symbol] = secondParamList.filter(p => p.isPayload || p.isManualTrivia)
    def payload: List[Symbol] = secondParamList.filter(p => p.isPayload)
    def allFields: List[Symbol] = secondParamList

    def asRoot: Root = { require(sym.isRoot); Root(sym) }
    def asBranch: Branch = { require(sym.isBranch); Branch(sym) }
    def asLeaf: Leaf = { require(sym.isLeaf); Leaf(sym) }
    def asField: Field = { require(sym.isTerm && sym.isParameter); Field(sym) }
  }

  trait NonLeafApi {
    def sym: Symbol
    def branches: List[Branch] = sym.branches.map(_.asBranch)
    def allBranches: List[Branch] = sym.allBranches.map(_.asBranch)
    def leafs: List[Leaf] = sym.leafs.map(_.asLeaf)
    def allLeafs: List[Leaf] = sym.allLeafs.map(_.asLeaf)
  }
  case class Root(sym: Symbol) extends NonLeafApi
  case class Branch(sym: Symbol) extends NonLeafApi
  case class Leaf(sym: Symbol) {
    def fields: List[Field] = sym.branches.map(_.asField)
    def payload: List[Field] = sym.branches.map(_.asField)
    def allFields: List[Field] = sym.branches.map(_.asField)
  }
  case class Field(sym: Symbol) {
    def name: TermName = TermName(sym.name.toString.stripPrefix("_"))
    def tpe: Type = sym.info
    def isManualTrivia: Boolean = sym.isManualTrivia
    def isAutoTrivia: Boolean = sym.isAutoTrivia
  }
}