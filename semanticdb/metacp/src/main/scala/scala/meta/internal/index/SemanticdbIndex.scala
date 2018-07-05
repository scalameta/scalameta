package scala.meta.internal.index

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.mutable
import scala.meta.internal.io._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io._

class SemanticdbIndex {
  private val toplevels = mutable.Map[String, String]()

  def append(uri: String, infos: List[s.SymbolInformation]): Unit = {
    infos.foreach { info =>
      toplevels(info.symbol) = uri
    }
  }

  def save(out: AbsolutePath): Unit = {
    save(out, None)
  }

  def save(out: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    save(out, Some(sourceroot))
  }

  private def save(out: AbsolutePath, sourceroot: Option[AbsolutePath]): Unit = {
    val indexAbspath = out.resolve("META-INF/semanticdb/semanticdb.index")
    val baseIndex = sourceroot match {
      case Some(root) if indexAbspath.isFile => getBaseIndex(indexAbspath, root)
      case _ => Map.empty[String, String]
    }
    val stoplevels = baseIndex ++ toplevels
    val indexMessage = s.Indexes(List(s.Index(stoplevels)))
    FileIO.write(indexAbspath, indexMessage)
  }

  private def getBaseIndex(indexPath: AbsolutePath, sourceroot: AbsolutePath): Map[String, String] = {
    val sourceuri = sourceroot.toURI
    val old = FileIO.readIndex(indexPath)
    val fileCache = mutable.Map.empty[String, Boolean]
    def isFile(uri: String): Boolean =
      fileCache.getOrElseUpdate(uri, {
        val absuri = sourceuri.resolve(uri.stripSuffix(".semanticdb"))
        Files.isRegularFile(Paths.get(absuri))
      })
    old.toplevels.filter {
      case (_, uri) => isFile(uri)
    }
  }
}
