package scala.meta.internal.scalacp

import org.scalameta.collections.Conversions._
import scala.meta.internal.classpath._
import scala.meta.internal.metacp._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal

final class SymbolIndex private (classpathIndex: ClasspathIndex) {
  private lazy val lookupCache = mutable.Map.empty[Symbol, SymbolLookup]
  def lookup(sym: ExternalSymbol): SymbolLookup = lookupCache.getOrElseUpdate(
    sym,
    if (sym.isRootPackage || sym.isEmptyPackage || classpathIndex.isClassdir(sym.packageResourceName))
      PackageLookup
    else if (sym.isScalalibSynthetic) ScalaLookup
    else sym.parent match {
      case Some(p: ExternalSymbol) => lookup(p) match {
          case PackageLookup =>
            val scalaShortName =
              (if (sym.entry.entryType == 10) sym.name + NameTransformer.MODULE_SUFFIX_STRING
               else sym.name) + Classfile.Suffix
            classpathIndex.getClassfile(p.packageResourceName, scalaShortName) match {
              case Some(entry) => if (entry.hasScalaSig) ScalaLookup else JavaLookup
              case None =>
                val javaShortName = sym.name + Classfile.Suffix
                classpathIndex.getClassfile(p.packageResourceName, javaShortName) match {
                  case Some(entry) => if (entry.hasScalaSig) MissingLookup else JavaLookup
                  case None => MissingLookup
                }
            }
          case otherLookup => otherLookup
        }
      case _ => MissingLookup
    },
  )

  /** Returns the names of the methods in the classfile of a class, in classfile order. */
  def methodNames(cls: ClassSymbol): IndexedSeq[String] = {
    val res = IndexedSeq.newBuilder[String]
    try classfile(cls).foreach(_.toClassNode.methods.forEach(m => res += m.name))
    catch { case NonFatal(_) => }
    res.result()
  }

  private def classfile(cls: ClassSymbol): Option[Classfile] = {
    @tailrec
    def loop(s: Symbol, names: List[String]): Option[Classfile] = s.parent match {
      case Some(p: ClassSymbol) => loop(p, p.name :: names)
      case Some(p: ExternalSymbol) =>
        val filename = names.mkString("", NameTransformer.NAME_JOIN_STRING, Classfile.Suffix)
        classpathIndex.getClassfile(p.packageResourceName, filename)
      case _ => None
    }
    val clsName = if (cls.isModule) cls.name + NameTransformer.MODULE_SUFFIX_STRING else cls.name
    loop(cls, clsName :: Nil)
  }

  private implicit class XtensionSymbolOps(sym: Symbol) {
    def isRootPackage: Boolean = sym.path == "<root>"
    def isEmptyPackage: Boolean = sym.path == "<empty>"
    def isScalalibSynthetic: Boolean = SymbolIndex.scalalibSyntheticsPaths.contains(sym.path)
    def packageResourceName: String = ownerChain.filterNot(_.isEmptyPackage).map(_.name)
      .mkString("", "/", "/")
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
}

object SymbolIndex {
  def apply(classpathIndex: ClasspathIndex): SymbolIndex = new SymbolIndex(classpathIndex)

  // the paths do not depend on the shape of `##`
  private lazy val scalalibSyntheticsPaths: Set[String] = Scalalib
    .synthetics(isHashHashNullaryInBuild)
    .map(synthetic => synthetic.relativeUri.stripSuffix(Classfile.Suffix).replace('/', '.')).toSet
}
