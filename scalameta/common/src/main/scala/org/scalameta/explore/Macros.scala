package org.scalameta
package explore

import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers
import scala.collection.mutable.Set

class ExploreMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  lazy val m = c.mirror

  def publicToplevelDefinitions(packageName: Tree): Tree = {
    val visited = Set[Symbol]()
    def explore(parent: Symbol): Unit = {
      if (parent == NoSymbol) return
      parent.info.members.toList.foreach(sym => {
        def failUnsupported(sym: Symbol): Nothing = {
          c.abort(c.enclosingPosition, s"unsupported symbol $sym: ${sym.fullName}")
        }
        if (sym.name.decodedName.toString.contains("$")) {
          // ignore: not worth processing
        } else {
          if (sym.isPackage) {
            visited += sym
            // don't recur, since members of that package won't be visible w/o imports
          } else if (sym.isModule) {
            if (!visited(sym)) explore(sym)
            visited += sym
            // do recur, because we expect members of modules to be used via full names
          } else if (sym.isClass) {
            if (!sym.isImplicit) visited += sym
          } else if (sym.isMethod) {
            if (sym.asMethod.isAccessor) {
              if (sym.asMethod.isGetter) {
                val target = sym.info.finalResultType.typeSymbol
                if (target.isModuleClass) explore(target.asClass.module)
                else () // ignore: we're not going into members in this macro
              } else {
                // ignore: setters are not worth processing
              }
            } else {
              val parentIsPackageObject = (parent.isModule || parent.isModuleClass) && parent.name.toString == "package"
              if (!sym.isImplicit && parentIsPackageObject) visited += sym
            }
          } else if (sym.isType) {
            val target = sym.info.typeSymbol
            if (!visited(target)) visited += target
          } else if (sym.isTerm) {
            if (sym.asTerm.getter != NoSymbol) {
              // ignore: processed in getter
            } else {
              failUnsupported(sym)
            }
          } else {
            failUnsupported(sym)
          }
        }
      })
    }

    val Literal(Constant(s_packageName: String)) = packageName
    explore(scala.util.Try(m.staticPackage(s_packageName)).getOrElse(NoSymbol))
    explore(scala.util.Try(m.staticModule(s_packageName + ".package")).getOrElse(NoSymbol))

    def isVisible(sym: Symbol): Boolean = sym != NoSymbol && sym.isPublic && (sym.isPackage || isVisible(sym.owner))
    val visible = visited.filter(isVisible)
    val nontrivial = visible.filter(sym => sym.owner != symbolOf[Any] && sym.owner != symbolOf[Object])
    val relevant = nontrivial.filter(sym => !sym.fullName.startsWith("scala.collection.generic"))
    val result = relevant

    val s_result = result.toList.map(sym => scala.reflect.NameTransformer.decode(sym.fullName)).distinct.sorted
    // s_result.foreach(println)
    q"$s_result"
  }

  def publicSurface(packageName: Tree): Tree = {
    ???
  }
}
