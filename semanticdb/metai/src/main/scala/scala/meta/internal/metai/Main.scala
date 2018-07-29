package scala.meta.internal.metai

import java.net.URI
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.meta.cli.Reporter
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.io._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.{semanticidx => i}
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.io.RelativePath
import scala.meta.metai.Settings

final class Main(settings: Settings, reporter: Reporter) {
  def process(): Classpath = {
    val buf = List.newBuilder[AbsolutePath]
    settings.classpath.foreach { entry =>
      try {
        if (processEntry(entry)) {
          buf += entry.path
        }
      } catch {
        case e: Throwable =>
          println(s"Error indexing $entry:")
          e.printStackTrace(reporter.err)
      }
    }
    Classpath(buf.result)
  }

  private def processEntry(file: ClasspathFile): Boolean = {
    var hasSemanticdb = false
    val ls = FileIO.listAllFilesRecursively(file.path)
    var index = i.Index()
    ls.files.foreach { relpath =>
      if (PathIO.extension(relpath.toNIO) == "semanticdb") {
        hasSemanticdb = true
        val entries = getEntriesForPath(ls.root, relpath).entries
        index = index.addAllEntries(entries)
      }
    }
    if (hasSemanticdb) {
      val indexes = i.Indexes(List(index))
      writeIndex(ls.root, indexes)
      true
    } else {
      file.path.toManifest match {
        case Some(manifest) if manifest.getMainAttributes.getValue("Class-Path") != null =>
          // It's expected that manifest jars that point to other jars don't have SemanticDB files.
          true
        case _ =>
          reporter.err.println(s"No SemanticDB: ${file.pathOnDisk}")
          false
      }
    }
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

  private val RelUriPrefix = URI.create("META-INF/semanticdb")

  private def getEntriesForPath(root: AbsolutePath, relpath: RelativePath): i.Index = {
    val abspath = root.resolve(relpath)
    val reluri = RelUriPrefix.relativize(relpath.toURI(isDirectory = false)).toString
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
