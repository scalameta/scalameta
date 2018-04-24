package scala.meta.internal.index

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
          val decls = packages.getOrElse(owner, mutable.Set[String]())
          decls += decl
          packages(owner) = decls
      }
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
