package scala.meta.internal.symtab

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.io._
import scala.meta.internal.metacp._
import scala.meta.internal.scalacp.Scalalib
import scala.meta.internal.semanticdb.Scala._
import scala.reflect.NameTransformer

/** A lazy symbol table that returns global symbols on-the-fly from disk. */
final class GlobalSymbolTable(classpathIndex: ClasspathIndex) extends SymbolTable {

  private val symbolCache = TrieMap.empty[String, SymbolInformation]
  Scalalib.synthetics.foreach(enter)

  override def toString: String = s"GlobalSymbolTable($classpathIndex)"

  private def enter(infos: ClassfileInfos): Unit =
    infos.infos.foreach { info =>
      symbolCache(info.symbol) = info
    }

  private def loadSymbol(symbol: String): Unit = {
    val toplevel = symbol.ownerChain.find(!_.isPackage).get
    val owner = toplevel.owner
    val classdir = if (owner.isEmptyPackage) "/" else owner
    val filename = NameTransformer.encode(toplevel.desc.name.decoded) + ".class"
    classpathIndex.getClassfile(classdir, filename) match {
      case Some(classfile) =>
        val node = classfile.toClassNode
        ClassfileInfos.fromClassNode(node, classpathIndex) match {
          case Some(infos) =>
            enter(infos)
          case _ =>
            ()
        }
      case _ =>
        ()
    }
  }

  override def info(symbol: String): Option[SymbolInformation] =
    if (symbol.isNone) None
    else if (symbol.isPackage) {
      if (symbol.isRootPackage || symbol.isEmptyPackage || classpathIndex.isClassdir(symbol)) {
        val info = SymbolInformation(
          symbol = symbol,
          name = symbol.desc.name,
          kind = SymbolInformation.Kind.PACKAGE
        )
        Some(info)
      } else {
        None
      }
    } else {
      symbolCache.get(symbol) match {
        case Some(x) =>
          Some(x)
        case None =>
          loadSymbol(symbol)
          symbolCache.get(symbol)
      }
    }
}

object GlobalSymbolTable {
  def apply(classpath: Classpath): GlobalSymbolTable = {
    new GlobalSymbolTable(ClasspathIndex(classpath))
  }
}
