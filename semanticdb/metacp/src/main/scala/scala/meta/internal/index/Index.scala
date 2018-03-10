package scala.meta.internal.index

import org.langmeta.internal.io._
import org.langmeta.io._
import scala.collection.mutable
import scala.meta.internal.{semanticdb3 => s}

class Index {
  private val packages = mutable.Map[String, mutable.Set[String]]()
  private val toplevels = mutable.Map[String, String]()
  packages("_root_.") = mutable.Set[String]()
  packages("_empty_.") = mutable.Set[String]()

  def append(uri: String, infos: List[s.SymbolInformation]): Unit = {
    infos.foreach { info =>
      toplevels(info.symbol) = uri
      if (info.symbol.stripSuffix("#").contains("#")) return
      val ownerChain = info.owner.split("\\.")
      ownerChain.scanLeft("") { (prefix, name) =>
        val ancestorSym = {
          if (prefix == "" && name != "_root_") "_root_."
          else prefix
        }
        val sym = prefix + name + "."
        val decls = packages.getOrElse(sym, mutable.Set[String]())
        packages(sym) = decls
        if (ancestorSym != "") packages(ancestorSym) += sym
        sym
      }
      packages(info.owner) += info.symbol
    }
  }

  def save(out: AbsolutePath): Unit = {
    val indexAbspath = out.resolve("META-INF").resolve("semanticdb.semanticidx")
    val indexMessage = {
      val spackages = packages.map(kv => s.PackageEntry(symbol = kv._1, members = kv._2.toList))
      val stoplevels = toplevels.map(kv => s.ToplevelEntry(symbol = kv._1, uri = kv._2))
      s.Index(packages = spackages.toList, toplevels = stoplevels.toList)
    }
    FileIO.write(indexAbspath, indexMessage)
  }
}
