package scala.meta.internal.index

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.collection.mutable
import scala.meta.internal.io._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.io._

class Index {
  private val packages = mutable.Map[String, mutable.Set[String]]()
  private val toplevels = mutable.Map[String, String]()
  packages("_root_.") = mutable.Set[String]()
  packages("_empty_.") = mutable.Set[String]()

  def append(uri: String, infos: List[s.SymbolInformation]): Unit = {
    infos.foreach { info =>
      toplevels(info.symbol) = uri
      info.symbol.ownerChain.sliding(2).foreach {
        case List(owner, decl) =>
          addPackageMember(owner, decl)
      }
    }
  }

  def save(out: AbsolutePath): Unit = {
    save(out, None)
  }
  def save(out: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    save(out, Some(sourceroot))
  }

  private def save(out: AbsolutePath, sourceroot: Option[AbsolutePath]): Unit = {
    val indexAbspath = out.resolve("META-INF").resolve("semanticdb.semanticidx")
    val baseIndex = sourceroot match {
      case Some(root) if indexAbspath.isFile => getBaseIndex(indexAbspath, root)
      case _ => s.Index()
    }
    val indexMessage = {
      val spackages = packages.map(kv => s.PackageEntry(symbol = kv._1, members = kv._2.toList))
      val stoplevels = toplevels.map(kv => s.ToplevelEntry(symbol = kv._1, uri = kv._2))
      baseIndex
        .addAllPackages(spackages)
        .addAllToplevels(stoplevels)
    }
    FileIO.write(indexAbspath, indexMessage)
  }

  private def addPackageMember(pkg: String, decl: String): Unit = {
    val decls = packages.getOrElseUpdate(pkg, mutable.Set[String]())
    decls += decl
  }

  private def getBaseIndex(indexPath: AbsolutePath, sourceroot: AbsolutePath): s.Index = {
    val in = Files.newInputStream(indexPath.toNIO)
    val old =
      try s.Index.parseFrom(indexPath.readAllBytes)
      finally in.close()
    val fileCache = mutable.Map.empty[String, Boolean]
    def isFile(uri: String): Boolean =
      fileCache.getOrElseUpdate(uri, {
        val relpath =
          URLDecoder.decode(uri, StandardCharsets.UTF_8.name()).stripSuffix(".semanticdb")
        val abspath = sourceroot.resolve(relpath)
        abspath.isFile
      })
    val newToplevels = old.toplevels.filter(t => isFile(t.uri) && !toplevels.contains(t.symbol))
    val isNewToplevel = newToplevels.iterator.map(_.symbol).toSet
    old.packages.foreach { pkg =>
      pkg.members.foreach { member =>
        if (isNewToplevel(member)) {
          addPackageMember(pkg.symbol, member)
        }
      }
    }
    s.Index(packages = Nil, toplevels = newToplevels)
  }
}
