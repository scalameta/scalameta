package scala.meta.internal.scalacp

import scala.collection.mutable
import scala.meta.internal.classpath._
import scala.meta.internal.metacp._
import scala.tools.scalap.scalax.rules.scalasig._

final class SymbolIndex private (classpathIndex: ClasspathIndex) {
  private lazy val lookupCache = mutable.Map.empty[Symbol, SymbolLookup]
  def lookup(sym: ExternalSymbol): SymbolLookup = {
    lookupCache.getOrElseUpdate(sym, {
      if (sym.isRootPackage ||
          sym.isEmptyPackage ||
          classpathIndex.isClassdir(sym.packageResourceName)) {
        PackageLookup
      } else if (sym.isScalalibSynthetic) {
        ScalaLookup
      } else {
        sym.parent match {
          case Some(p: ExternalSymbol) =>
            lookup(p) match {
              case PackageLookup =>
                val scalaShortName = {
                  if (sym.entry.entryType == 10) sym.name + "$.class"
                  else sym.name + ".class"
                }
                classpathIndex.getClassfile(p.packageResourceName, scalaShortName) match {
                  case Some(entry) =>
                    if (entry.hasScalaSig) ScalaLookup
                    else JavaLookup
                  case None =>
                    val javaShortName = sym.name + ".class"
                    classpathIndex.getClassfile(p.packageResourceName, javaShortName) match {
                      case Some(entry) =>
                        if (entry.hasScalaSig) MissingLookup
                        else JavaLookup
                      case None =>
                        MissingLookup
                    }
                }
              case otherLookup =>
                otherLookup
            }
          case _ =>
            MissingLookup
        }
      }
    })
  }

  private implicit class XtensionSymbolOps(sym: Symbol) {
    def isRootPackage: Boolean = sym.path == "<root>"
    def isEmptyPackage: Boolean = sym.path == "<empty>"
    def isScalalibSynthetic: Boolean = {
      scalalibSyntheticsPaths.contains(sym.path)
    }
    def packageResourceName: String = {
      ownerChain.filterNot(_.isEmptyPackage).map(_.name).mkString("", "/", "/")
    }
    private def ownerChain: List[Symbol] = {
      val buf = List.newBuilder[Symbol]
      def loop(s: Symbol): Unit = {
        s.parent.foreach(loop)
        buf += s
      }
      loop(sym)
      buf.result()
    }
  }
  private lazy val scalalibSyntheticsPaths: Set[String] =
    Scalalib.synthetics.map { synthetic =>
      synthetic.relativeUri.stripSuffix(".class").replace('/', '.')
    }.toSet
}

object SymbolIndex {
  def apply(classpathIndex: ClasspathIndex): SymbolIndex = {
    new SymbolIndex(classpathIndex)
  }
}
