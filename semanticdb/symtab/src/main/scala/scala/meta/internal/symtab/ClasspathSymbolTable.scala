package scala.meta.internal.symtab

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.io._
import scala.meta.internal.metacp._
import scala.meta.internal.scalacp.Scalalib
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scope
import scala.reflect.NameTransformer

final class ClasspathSymbolTable(classpathIndex: ClasspathIndex) extends SymbolTable {
  private val symbolCache = TrieMap.empty[String, SymbolInformation]
  Scalalib.synthetics.foreach(enter)

  override def toString: String = s"ClasspathSymbolTable($classpathIndex)"

  private def enter(infos: ClassfileInfos): Unit =
    infos.infos.foreach { info =>
      symbolCache(info.symbol) = info
    }

  @tailrec private def getToplevel(symbol: String): String = {
    val owner = symbol.owner
    if (owner.isPackage) symbol
    else getToplevel(owner)
  }

  private def loadSymbol(symbol: String): Unit = {
    val toplevel = getToplevel(symbol)
    val classdir = toplevel.owner
    val filename = NameTransformer.encode(toplevel.desc.name.decoded) + ".class"
    classpathIndex.getClassfile(classdir, filename) match {
      case Some(classfile) =>
        val node = classfile.toClassNode
        ClassfileInfos.fromClassNode(node, node.name + ".class", classpathIndex) match {
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
    if (symbol.isPackage) {
      if (classpathIndex.isClassdir(symbol)) {
        Some(SymbolInformation(symbol, kind = SymbolInformation.Kind.PACKAGE))
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

  override def withScope(scope: Scope): SymbolTable =
    LocalSymbolTable(this, scope)
}

object ClasspathSymbolTable {
  def fromClasspath(classpath: Classpath): ClasspathSymbolTable = {
    new ClasspathSymbolTable(ClasspathIndex(classpath))
  }
}
