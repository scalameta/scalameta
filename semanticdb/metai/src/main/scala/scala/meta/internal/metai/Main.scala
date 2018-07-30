package scala.meta.internal.metai

import java.net.URI
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.collection.immutable
import scala.collection.mutable
import scala.meta.cli.Reporter
import scala.meta.internal.cli._
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
import scala.meta.metai.Result
import scala.meta.metai.Settings

final class Main(settings: Settings, reporter: Reporter) {
  def process(): Result = {
    val status = mutable.Map[AbsolutePath, Boolean]()
    val job = Job(settings.classpath.entries, if (settings.verbose) reporter.err else devnull)
    job.foreach { entry =>
      var success = true
      try {
        Classpath(entry).foreach { entry =>
          success &= processEntry(entry)
        }
      } catch {
        case e: Throwable =>
          reporter.err.println(s"Error indexing $entry:")
          e.printStackTrace(reporter.err)
          success = false
      }
      status(entry) = success
    }
    reporter.out.println("{")
    reporter.out.println("  \"status\": {")
    val ins = settings.classpath.entries
    ins.zipWithIndex.foreach {
      case (in, i) =>
        reporter.out.print(s"""    "${in.toNIO}": ${status(in)}""")
        if (i != ins.length - 1) reporter.out.print(",")
        reporter.out.println()
    }
    reporter.out.println("  }")
    reporter.out.println("}")
    Result(immutable.ListMap(ins.map(in => in -> status(in)): _*))
  }

  private def processEntry(file: ClasspathFile): Boolean = {
    val hasSemanticdb = file.path.resolve("META-INF").resolve("semanticdb").isDirectory
    val ls = FileIO.listAllFilesRecursively(file.path)
    var index = i.Index()
    ls.files.foreach { relpath =>
      if (PathIO.extension(relpath.toNIO) == "semanticdb") {
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
          reporter.err.println(s"No META-INF/semanticdb found in ${file.pathOnDisk}")
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
