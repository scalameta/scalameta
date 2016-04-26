package org.scalameta
package explore

import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers
import scala.collection.mutable.Set

class ExploreMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  lazy val m = c.mirror

  private def discoverPublicToplevelDefinitions(pkg: Symbol): List[Symbol] = {
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
            // do recur, because we expect many members of modules to be used via full names
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
              if (!sym.isImplicit && !sym.isConstructor && parentIsPackageObject) visited += sym
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

    assert(pkg.isPackage)
    explore(pkg)
    explore(pkg.info.member(termNames.PACKAGE))

    def isVisible(sym: Symbol): Boolean = sym != NoSymbol && sym.isPublic && (sym.isPackage || isVisible(sym.owner))
    val visible = visited.filter(isVisible)
    val nontrivial = visible.filter(sym => sym.owner != symbolOf[Any] && sym.owner != symbolOf[Object])
    val relevant = nontrivial.filter(sym => {
      val mystery = sym.fullName.startsWith("scala.collection.generic")
      val tests = sym.fullName.contains(".tests.") || sym.fullName.endsWith("tests")
      !mystery && !tests
    })
    val result = relevant

    result.toList
  }

  private def render(strings: List[String]): Tree = {
    val s_result = strings.distinct.sorted
    // s_result.foreach(println)
    q"$s_result"
  }

  def publicToplevelDefinitions(packageName: Tree): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName
    val tlds = discoverPublicToplevelDefinitions(m.staticPackage(s_packageName))
    val names = tlds.map(sym => scala.reflect.NameTransformer.decode(sym.fullName))
    render(names)
  }

  def publicExtensionMethods(packageName: Tree): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName
    val tlds = discoverPublicToplevelDefinitions(m.staticPackage(s_packageName))
    val implicitClasses = tlds.flatMap(tld => {
      if (tld.isModule) tld.info.members.filter(sym => sym.isClass && sym.isImplicit && sym.isPublic)
      else Seq()
    })
    val signatures = implicitClasses.flatMap(cls => {
      def signature(m: Symbol, wrapFirst: Boolean) = {
        val pss = m.asMethod.paramLists
        val s_ps = pss.zipWithIndex.map{ case (ps, i) =>
          val s_p = ps.map(_.info).mkString(", ")
          val designator = if (ps.exists(_.isImplicit)) "implicit " else ""
          val prefix = if (i != 0 || wrapFirst) "(" else ""
          val suffix = if (i != 0 || wrapFirst) ")" else ""
          prefix + designator + s_p + suffix
        }
        s_ps.mkString("")
      }
      val ctor = cls.info.decls.find(sym => sym.isMethod && sym.asMethod.isPrimaryConstructor).get
      val extendee = signature(ctor, wrapFirst = false)
      val extensionMethods = cls.info.decls.collect{ case sym if sym.isMethod && !sym.isConstructor => sym.asMethod }
      val visible = extensionMethods.filter(_.isPublic)
      visible.map(m => extendee + "." + m.name.decodedName.toString + signature(m, wrapFirst = true))
    })
    render(signatures)
  }

  def publicSurface(packageName: Tree): Tree = {
    ???
  }
}
