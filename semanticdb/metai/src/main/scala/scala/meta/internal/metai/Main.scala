package scala.meta.internal.metai

import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import scala.collection.mutable
import scala.meta.cli.Reporter
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.{semanticidx => i}
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.io._
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.io.RelativePath
import scala.meta.metai.Settings
import scala.util.control.NonFatal

final class Main(settings: Settings, reporter: Reporter) {
  def process(): Option[Classpath] = {
    var success = true
    settings.classpath.foreach { entry =>
      try {
        processEntry(entry)
      } catch {
        case NonFatal(e) =>
          e.printStackTrace(reporter.err)
          success = false
      }
    }
    if (success) {
      Some(settings.classpath)
    } else {
      None
    }
  }

  private def processEntry(entry: AbsolutePath): Unit = {
    val semanticdb = entry.resolve("META-INF").resolve("semanticdb")
    if (!semanticdb.isDirectory) return
    val ls = FileIO.listAllFilesRecursively(semanticdb)
    val index = mutable.Map.empty[String, i.Entry]
    ls.files.foreach { relpath =>
      if (PathIO.extension(relpath.toNIO) == "semanticdb") {
        updateIndex(index, ls.root, relpath)
      }
    }
    val indexes = i.Indexes(List(i.Index(index.toMap)))
    writeIndex(ls.root, indexes)
  }

  private def writeIndex(root: AbsolutePath, indexes: i.Indexes): Unit = {
    val path = root.resolve("META-INF").resolve("semanticdb.semanticidx")
    PlatformFileIO.write(
      path,
      indexes,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }

  private def updateIndex(
      index: mutable.Map[String, i.Entry],
      root: AbsolutePath,
      relpath: RelativePath
  ): Unit = {
    val abspath = root.resolve(relpath)
    val reluri = relpath.toURI(isDirectory = false).toString
    val in = Files.newInputStream(abspath.toNIO)
    val docs =
      try s.TextDocuments.parseFrom(in)
      finally in.close()
    updateIndex(index, reluri, docs)
  }

  private def updateIndex(
      index: mutable.Map[String, i.Entry],
      reluri: String,
      docs: s.TextDocuments
  ): Unit = {
    val PackageEntry = i.PackageEntry()
    for {
      doc <- docs.documents
      toplevel <- doc.symbols
      if toplevel.symbol.owner.isPackage
    } {
      toplevel.symbol.ownerChain.foreach { sym =>
        val entry =
          if (sym.isPackage) PackageEntry
          else i.ToplevelEntry(reluri)
        index(sym) = entry
      }
    }
  }

}
