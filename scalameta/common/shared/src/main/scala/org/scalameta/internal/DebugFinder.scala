package org.scalameta
package internal

import scala.reflect.macros.blackbox.Context

trait DebugFinder {
  val c: Context
  import c.universe._

  def debuggees(tree: Tree): Map[String, Tree] = {
    object debugFinder extends Traverser {
      private val invariantsPackageObject = c.mirror.staticPackage("org.scalameta").info.member(termNames.PACKAGE).asModule
      private val invariantsDebug = invariantsPackageObject.info.member(TermName("debug")).asMethod
      val debuggees = scala.collection.mutable.ListBuffer[Tree]()
      override def traverse(tree: Tree): Unit = tree match {
        case Apply(fun, args) if fun.symbol == invariantsDebug => debuggees ++= args
        case tree => super.traverse(tree)
      }
    }
    debugFinder.traverse(tree)
    debugFinder.debuggees.map(tree => tree.toString -> tree.duplicate).toMap
  }
}
