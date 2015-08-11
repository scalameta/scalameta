package scala.meta
package taxonomic

import java.io._
import java.net.URI
import java.util.zip._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.immutable.ListMap
import scala.collection.{immutable, mutable}
import scala.meta.taxonomic.{Context => TaxonomicContext}
import org.apache.ivy.plugins.resolver._
import org.scalameta.contexts._
import org.scalameta.invariants._

// NOTE: I've been thinking a lot whether I can put this class here, effectively proclaiming
// that classpath-based modules, possibly hosted via maven, are the standard.
// It is the standard now, but obviously once we have new and exciting language dialects
// or maybe additional backends, the situation may change.
//
// Over the last week, I've made a few design attempts that involved abstracting
// all kinds of details that may be deemed platform-specific. I'll mention two prominent results here.
//
// 1) Taxonomic contexts are implemented in hosts, and every host requires a taxonomic context to
// instantiate a platform-dependent semantic context. All was fine until I realized that one may
// erroneously mix taxonomic and semantic contexts from different platforms, which will lead to fail.
//
// 2) Taxonomic contexts are removed, and Module becomes a plain data structure that carries around
// everything that might be needed from it (path to binaries, sequence of sources, etc).
// With enough pressure, it is possible to even make such modules lazy, but then the data model
// becomes so bland that it's just stupid - there's no way to create different kinds of modules,
// there's no way to encapsulate anything, there's even no way to create custom platform-dependent tostrings!
//
// All in all, I was left really unsatisfied after all these attempts to abstract away something
// that's hasn't even materialized yet. Therefore, I've decided to hardcode the JVM-based reality for now
// and deal with the possible future once it actually happens.

@context(translateExceptions = true) case class Taxonomy(resolvers: DependencyResolver*) extends TaxonomicContext {
  private case class ResolvedModule(binaries: Seq[Path], sources: Seq[Source], resources: Seq[Resource], deps: Seq[Module])
  private val cache = mutable.Map[Module, ResolvedModule]()

  private def resolveUnmanaged(module: Artifact.Unmanaged): ResolvedModule = cache.getOrElseUpdate(module, {
    implicit class XtensionPath(path: Path) {
      def explode: ListMap[String, URI] = {
        val root = new File(path.path)
        val result = mutable.ListMap[String, URI]()
        def addFile(file: File): Unit = {
          val relativePath = {
            require(file.getPath.startsWith(root.getPath) && debug(file, root))
            var result = file.getPath.stripPrefix(root.getPath)
            if (result.startsWith("/")) result = result.substring(1)
            result
          }
          result(relativePath) = file.toURI
        }
        def addZipEntry(file: File, entry: ZipEntry): Unit = {
          var relativePath = entry.getName
          if (relativePath.startsWith("/")) relativePath = relativePath.substring(1)
          if (relativePath.endsWith("/")) return
          result(relativePath) = new URI("jar:" + file.toURI.toURL + "!" + entry.getName)
        }
        def explore(file: File): Unit = {
          if (file.isDirectory) {
            val files = file.listFiles
            if (files != null) {
              files.filter(_.isFile).foreach(addFile)
              files.filter(_.isDirectory).foreach(explore)
            }
          } else if (file.getName.endsWith(".jar")) {
            val stream = new FileInputStream(file)
            try {
              val zip = new ZipInputStream(stream)
              var result = ListMap[String, URI]()
              var entry = zip.getNextEntry()
              while (entry != null) {
                addZipEntry(file, entry)
                entry = zip.getNextEntry()
              }
            } finally {
              stream.close()
            }
          } else {
            addFile(file)
          }
        }
        explore(root)
        ListMap(result.toList: _*)
      }
    }
    implicit class XtensionMultipath(multipath: Multipath) {
      def explode: ListMap[String, URI] = ListMap(multipath.paths.flatMap(_.explode): _*)
    }
    val binaries @ (mainBinary +: depBinaries) = module.binpath.paths.toList
    val sources = {
      val classfiles = mainBinary.explode.filter(x => x._2.toString.endsWith(".class") && !x._2.toString.contains("$"))
      val sourcefiles = module.sourcepath.explode.filter(_._2.toString.endsWith(".scala"))
      if (sys.props("tasty.debug") != null) {
        println(classfiles)
        println(sourcefiles)
      }
      ???
    }
    val resources = Nil // TODO: we can definitely do better, e.g. by indexing all non-class files
    val deps = depBinaries.map(bin => Artifact(Multipath(bin), module.sourcepath)).toList // TODO: better heuristic?
    ResolvedModule(binaries, sources, resources, deps)
  })

  private def resolveMaven(module: Artifact.Maven): ResolvedModule = cache.getOrElseUpdate(module, {
    // NOTE: Parts of this file are originally taken from lihaoyi/ammonite:
    // https://github.com/lihaoyi/Ammonite/blob/cd5de73b5601735093f4f80a775423b7a0102b37/repl/src/main/scala/ammonite/repl/IvyThing.scala
    ???
  })

  def binaries(module: Module): Seq[Path] = module match {
    case Module.Adhoc(_, _, _) =>
      Nil
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).binaries
    case module: Artifact.Maven =>
      resolveMaven(module).binaries
  }

  def sources(module: Module): Seq[Source] = module match {
    case Module.Adhoc(sources, _, _) =>
      sources
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).sources
    case module: Artifact.Maven =>
      resolveMaven(module).sources
  }

  def resources(module: Module): Seq[Resource] = module match {
    case Module.Adhoc(_, resources, _) =>
      resources
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).resources
    case module: Artifact.Maven =>
      resolveMaven(module).resources
  }

  def deps(module: Module): Seq[Module] = module match {
    case Module.Adhoc(_, _, deps) =>
      deps
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).deps
    case module: Artifact.Maven =>
      resolveMaven(module).deps
  }
}