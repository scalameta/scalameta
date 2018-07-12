package scala.meta.internal.metai

import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.collection.mutable
import scala.meta.cli.Reporter
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.io._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.{semanticidx => i}
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import scala.meta.metai.Settings
import scala.util.control.NonFatal

final class Main(settings: Settings, reporter: Reporter) {
  def process(): Boolean = {
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
    success
  }

  private def processEntry(entry: AbsolutePath): Unit = {
    val semanticdb = entry.resolve("META-INF").resolve("semanticdb")
    if (!semanticdb.isDirectory) return
    val ls = FileIO.listAllFilesRecursively(semanticdb)
    var index = i.Index()
    ls.files.foreach { relpath =>
      if (PathIO.extension(relpath.toNIO) == "semanticdb") {
        val entries = getEntriesForPath(ls.root, relpath).entries
        index = index.addAllEntries(entries)
      }
    }
    val indexes = i.Indexes(List(index))
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

  private def getEntriesForPath(root: AbsolutePath, relpath: RelativePath): i.Index = {
    val abspath = root.resolve(relpath)
    val reluri = relpath.toURI(isDirectory = false).toString
    val in = Files.newInputStream(abspath.toNIO)
    val docs =
      try s.TextDocuments.parseFrom(in)
      finally in.close()
    getEntriesForDocs(reluri, docs)
  }

  private def getEntriesForDocs(reluri: String, docs: s.TextDocuments): i.Index = {
    val PackageEntry = i.PackageEntry()
    val entries = Map.newBuilder[String, i.Entry]
    for {
      doc <- docs.documents
      toplevel <- doc.symbols
      if !toplevel.symbol.isPackage
      if toplevel.symbol.owner.isPackage
    } {
      toplevel.symbol.ownerChain.foreach { sym =>
        val entry =
          if (sym.isPackage) PackageEntry
          else i.ToplevelEntry(reluri)
        entries += (sym -> entry)
      }
    }
    i.Index(entries.result())
  }

}
