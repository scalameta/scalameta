package org.scalameta
package explore

import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers
import scala.collection.mutable.Set

class ExploreMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  lazy val m = c.mirror

  private implicit class XtensionExploreSymbol(sym: Symbol) {
    def isIrrelevant: Boolean = {
      val trivial = sym.owner == symbolOf[Any] || sym.owner == symbolOf[Object]
      val arbitrary = sym.fullName.startsWith("scala.collection.generic")
      val tests = sym.fullName.contains(".tests.") || sym.fullName.endsWith("tests")
      val internal = sym.fullName.contains(".internal.") // NOTE: don't ignore "internal" itself
      val invisible = !sym.isPublic
      trivial || arbitrary || tests || internal || invisible
    }
    def isRelevant: Boolean = {
      !isIrrelevant
    }
    def signature: String = {
      def paramss(m: Symbol, wrapFirst: Boolean): String = {
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
      require(sym.isMethod)
      val prefix = {
        if (sym.owner.isImplicit) {
          val ctor = sym.owner.info.members.find(sym => sym.isMethod && sym.asMethod.isPrimaryConstructor).get
          paramss(ctor, wrapFirst = false)
        } else {
          sym.owner.name.decodedName.toString
        }
      }
      prefix + "." + sym.name.decodedName.toString + paramss(sym, wrapFirst = true) + ": " + sym.info.finalResultType
    }
  }

  private def discoverPublicToplevels(pkg: Symbol, onlyImmediatelyAccessible: Boolean): List[Symbol] = {
    val visited = Set[Symbol]()
    def loop(parent: Symbol): Unit = {
      if (parent == NoSymbol) return
      parent.info.members.toList.foreach(sym => {
        def failUnsupported(sym: Symbol): Nothing = {
          c.abort(c.enclosingPosition, s"unsupported symbol $sym: ${sym.fullName}")
        }
        if (sym.name.decodedName.toString.contains("$")) {
          // ignore: not worth processing
        } else {
          if (sym.isPackage) {
            if (!onlyImmediatelyAccessible && !visited(sym)) {
              visited += sym
              loop(sym)
            }
            visited += sym
          } else if (sym.isModule) {
            // don't check onlyImmediatelyAccessible
            // because we expect many members of modules to be used via full fullNames
            if (!visited(sym)) {
              visited += sym
              loop(sym)
            }
          } else if (sym.isClass) {
            visited += sym
          } else if (sym.isMethod) {
            // handle term aliases
            if (sym.asMethod.isGetter) {
              val target = sym.info.finalResultType.typeSymbol
              if (target.isModuleClass) loop(target.asClass.module)
              else ()
            }
          } else if (sym.isType) {
            // handle type aliases
            val target = sym.info.typeSymbol
            visited += target
          } else if (sym.isTerm) {
            if (sym.asTerm.getter != NoSymbol) {
              // ignore: processed in sym.isMethod
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
    loop(pkg)

    var result = visited.toList
    result = result.filter(sym => sym.isRelevant)
    result = result.filter(sym => !(sym.isClass && sym.isImplicit)) // NOTE: if we include implicit classes, we'll get overwhelmed
    result
  }

  private def render(strings: List[String]): Tree = {
    val s_result = strings.distinct.sorted
    // s_result.foreach(println)
    q"$s_result"
  }

  def wildcardImportToplevels(packageName: Tree): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName
    val toplevels = discoverPublicToplevels(m.staticPackage(s_packageName), onlyImmediatelyAccessible = true)
    val pkgobjectMethods = m.staticModule(s_packageName + ".package").info.members.toList.collect{
      case sym: MethodSymbol if !sym.isAccessor && !sym.isImplicit && !sym.isConstructor && sym.isRelevant => sym
    }
    val effectiveToplevels = toplevels ++ pkgobjectMethods
    val fullNames = effectiveToplevels.map(sym => scala.reflect.NameTransformer.decode(sym.fullName))
    render(fullNames)
  }

  def wildcardImportExtensions(packageName: Tree): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName
    val toplevels = discoverPublicToplevels(m.staticPackage(s_packageName), onlyImmediatelyAccessible = true)
    val implicitClasses = toplevels.flatMap(tl => {
      if (tl.isModule) tl.info.members.filter(sym => sym.isClass && sym.isImplicit && sym.isRelevant)
      else Seq()
    })
    val signatures = implicitClasses.flatMap(cls => {
      val extensionMethods = cls.info.decls.collect{ case sym if sym.isMethod && !sym.isConstructor => sym.asMethod }
      extensionMethods.filter(_.isRelevant).map(_.signature)
    })
    render(signatures)
  }

  def entireSurface(packageName: Tree): Tree = {
    ???
  }
}
