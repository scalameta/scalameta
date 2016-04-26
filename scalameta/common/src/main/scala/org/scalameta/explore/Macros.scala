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
      val arbitrary = {
        // TODO: figure out why this is necessary
        val banned = List("scala.collection.generic", "scala.Enumeration", "scala.math", "scala.Int")
        banned.exists(prefix => sym.fullName.startsWith(prefix))
      }
      val tests = sym.fullName.contains(".tests.") || sym.fullName.endsWith("tests")
      val internal = sym.fullName.contains(".internal.") // NOTE: don't ignore "internal" itself
      val invisible = !sym.isPublic
      val inexistent = !sym.asInstanceOf[scala.reflect.internal.SymbolTable#Symbol].exists // NOTE: wtf
      val result = trivial || arbitrary || tests || internal || invisible || inexistent
      // println((sym.fullName, s"$result = $trivial || $arbitrary || $tests || $internal || $invisible || $inexistent"))
      result
    }
    def isRelevant: Boolean = {
      !isIrrelevant
    }
    def isPkgObject: Boolean = {
      sym.owner.isPackage && sym.isModule && sym.name == termNames.PACKAGE
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

  private def staticClassesAndObjects(pkg: Symbol, onlyImmediatelyAccessible: Boolean): List[Symbol] = {
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
            if (sym.asTerm.getter != NoSymbol || sym.isPrivateThis) {
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

  private def staticsImpl(packageName: Tree, onlyImmediatelyAccessible: Boolean): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName
    val statics = staticClassesAndObjects(m.staticPackage(s_packageName), onlyImmediatelyAccessible)
    val (pkgObjects, nonPkgObjectStatics) = statics.partition(_.isPkgObject)
    val allowedPkgObjects = pkgObjects.filter(sym => sym.owner.fullName == s_packageName || !onlyImmediatelyAccessible)
    val pkgObjectMethods = allowedPkgObjects.flatMap(_.info.members.toList.collect{
      case sym: MethodSymbol if !sym.isAccessor && !sym.isImplicit && !sym.isConstructor && sym.isRelevant => sym
    })
    val effectiveStatics = nonPkgObjectStatics ++ pkgObjectMethods
    val fullNames = effectiveStatics.map(sym => scala.reflect.NameTransformer.decode(sym.fullName))
    q"${fullNames.distinct.sorted}"
  }

  def wildcardImportStaticsImpl(packageName: Tree): Tree = {
    staticsImpl(packageName, onlyImmediatelyAccessible = true)
  }

  def allStaticsImpl(packageName: Tree): Tree = {
    staticsImpl(packageName, onlyImmediatelyAccessible = false)
  }
}
