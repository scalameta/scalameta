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
      def artefact = sym.name.decodedName.toString.contains("$")
      def trivial = sym.owner == symbolOf[Any] || sym.owner == symbolOf[Object]
      def arbitrary = {
        // TODO: figure out why this is necessary
        val banned = List("scala.collection.generic", "scala.Enumeration", "scala.math", "scala.Int", "scala.meta.inline.Api")
        banned.exists(prefix => sym.fullName.startsWith(prefix))
      }
      def tests = sym.fullName.contains(".tests.") || sym.fullName.endsWith("tests")
      def internal = sym.fullName.contains(".internal.") // NOTE: don't ignore "internal" itself
      def invisible = !sym.isPublic
      def inexistent = !sym.asInstanceOf[scala.reflect.internal.SymbolTable#Symbol].exists // NOTE: wtf
      val result = artefact || trivial || arbitrary || tests || internal || invisible || inexistent
      // println((sym.fullName, s"$result = $artefact || $trivial || $arbitrary || $tests || $internal || $invisible || $inexistent"))
      result
    }
    def isRelevant: Boolean = {
      !isIrrelevant
    }
    def isPkgObject: Boolean = {
      sym.owner.isPackage && sym.isModule && sym.name == termNames.PACKAGE
    }
    def isImplicitClass: Boolean = {
      sym.isClass && sym.isImplicit
    }
    def signatureIn(owner: Symbol): String = {
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
      val prefix = {
        if (sym.owner.isImplicit) {
          val ctor = sym.owner.info.members.find(sym => sym.isMethod && sym.asMethod.isPrimaryConstructor).get
          "* " + paramss(ctor, wrapFirst = false)
        } else {
          scala.reflect.NameTransformer.decode(owner.fullName.toString)
        }
      }
      val main = sym.name.decodedName.toString
      val suffix = {
        if (sym.isConstructor || sym.isMacro || sym.isMethod) paramss(sym, wrapFirst = true) + ": " + sym.info.finalResultType
        else ""
      }
      prefix + "." + main + suffix
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
    result = result.filter(_.isRelevant)
    result
  }

  private def staticsImpl(packageName: Tree, onlyImmediatelyAccessible: Boolean): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName

    val statics = staticClassesAndObjects(m.staticPackage(s_packageName), onlyImmediatelyAccessible)
    val nonImplicitClassStatics = statics.filter(!_.isImplicitClass)
    val (pkgObjects, nonPkgObjectStatics) = nonImplicitClassStatics.partition(_.isPkgObject)
    val allowedPkgObjects = pkgObjects.filter(sym => sym.owner.fullName == s_packageName || !onlyImmediatelyAccessible)
    val pkgObjectMethods = allowedPkgObjects.flatMap(_.info.members.toList.collect{
      case sym: MethodSymbol if !sym.isAccessor && !sym.isImplicit && !sym.isConstructor && sym.isRelevant => sym
    })

    // NOTE: We filtered out package objects and implicit classes (hopefully, IDEs and autocompletes will ignore those),
    // and then we added methods from package objects (because `import scala.meta._` will bring those in).
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

  def allSurfaceImpl(packageName: Tree): Tree = {
    val Literal(Constant(s_packageName: String)) = packageName
    val statics = staticClassesAndObjects(m.staticPackage(s_packageName), onlyImmediatelyAccessible = false)

    val directSurface = statics.flatMap(static => {
      var result = static.info.members.filter(mem => {
        val relevant = mem.isRelevant
        val extension = static.isImplicitClass || mem.isImplicitClass
        val synthetic = mem.isConstructor && ((static.isClass && static.asClass.isTrait) || static.isModule || static.isModuleClass)
        val unnameable = mem.isMethod && mem.isImplicit // NOTE: this is a big assumption
        relevant && !extension && !synthetic && !unnameable
      })
      result = result.filter(mem => !statics.contains(mem))
      result.map(sym => (static, sym))
    })
    val extensionSurface = {
      val implicitClasses = statics.filter(_.isImplicitClass)
      val result = implicitClasses.flatMap(cls => {
        val extensionMethods = cls.info.decls.collect{ case sym if sym.isMethod && !sym.isConstructor => sym.asMethod }
        extensionMethods.filter(_.isRelevant)
      })
      result.map(sym => (NoSymbol, sym))
    }
    val s_surface = (directSurface ++ extensionSurface).map{ case (owner, sym) => sym.signatureIn(owner) }
    q"${s_surface.distinct.sorted}"
  }
}
