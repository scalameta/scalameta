package org.scalameta
package internal

import scala.reflect.macros.blackbox.Context

trait FreeLocalFinder {
  val c: Context
  import c.universe._
  import c.internal._
  import decorators._

  def freeLocals(tree: Tree): Map[String, Tree] = {
    object freeLocalFinder extends Traverser {
      private val localRefs = scala.collection.mutable.ListBuffer[Tree]()
      private val localSyms = scala.collection.mutable.Set[Symbol]()
      def registerLocalSym(sym: Symbol) {
        if (sym != null && sym != NoSymbol) localSyms += sym
      }
      def processLocalDef(tree: Tree) {
        if (tree.symbol != null && tree.symbol != NoSymbol) {
          val sym = tree.symbol
          registerLocalSym(sym)
          registerLocalSym(sym.deSkolemize)
          registerLocalSym(sym.companion)
          sym match {
            case sym: ClassSymbol => registerLocalSym(sym.module)
            case sym: ModuleSymbol => registerLocalSym(sym.moduleClass)
            case _ => ;
          }
        }
      }
      override def traverse(tree: Tree): Unit = {
        val sym = tree.symbol
        tree match {
          case tree: Ident if !sym.owner.isClass && tree.name != termNames.WILDCARD && !sym.isMethod => localRefs += tree
          case tree: This if !sym.isPackageClass => localRefs += tree
          case _: DefTree | Function(_, _) | Template(_, _, _) => processLocalDef(tree); super.traverse(tree)
          case _ => super.traverse(tree)
        }
      }
      def freeLocals = localRefs.filter(ref => !localSyms.contains(ref.symbol))
    }
    freeLocalFinder.traverse(tree)
    freeLocalFinder.freeLocals.map(tree => tree.symbol.name.toString -> tree.duplicate).toMap
  }
}