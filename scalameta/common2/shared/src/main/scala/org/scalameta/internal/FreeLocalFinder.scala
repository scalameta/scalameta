package org.scalameta
package internal

import scala.reflect.macros.blackbox.Context

trait FreeLocalFinder {
  val c: Context

  import c.internal._
  import c.internal.decorators._
  import c.universe._

  def freeLocals(tree: Tree): Map[String, Tree] = {
    object freeLocalFinder extends Traverser {
      private val localRefs = scala.collection.mutable.ListBuffer[Tree]()
      private val localSyms = scala.collection.mutable.Set[Symbol]()
      private def registerLocalSym(s: Symbol) = if (s != null && s != NoSymbol) localSyms += s
      private def processLocalDef(sym: Symbol) = if (sym != null && sym != NoSymbol) {
        registerLocalSym(sym)
        registerLocalSym(sym.deSkolemize)
        registerLocalSym(sym.companion)
        sym match {
          case sym: ClassSymbol => registerLocalSym(sym.module)
          case sym: ModuleSymbol => registerLocalSym(sym.moduleClass)
          case _ =>
        }
      }
      override def traverse(tree: Tree): Unit = {
        val sym = tree.symbol
        tree match {
          case tree: Ident => if (tree.name != termNames.WILDCARD)
              if (!sym.owner.isClass && !sym.isMethod) localRefs += tree
          case _: This => if (!sym.isPackageClass) localRefs += tree
          case _: DefTree | _: Function | _: Template =>
            processLocalDef(tree.symbol)
            super.traverse(tree)
          case _ => super.traverse(tree)
        }
      }
      def freeLocals: Map[String, Tree] = {
        val builder = Map.newBuilder[String, Tree]
        localRefs.foreach { ref =>
          val sym = ref.symbol
          if (!localSyms.contains(sym)) builder += sym.name.toString -> ref.duplicate
        }
        builder.result()
      }
    }
    freeLocalFinder.traverse(tree)
    freeLocalFinder.freeLocals
  }
}
