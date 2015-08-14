package scala.meta
package taxonomic

import java.io._
import java.net.URI
import java.nio.charset.Charset
import java.security.MessageDigest
import java.util.zip._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.immutable.ListMap
import scala.collection.{immutable, mutable}
import scala.reflect.{classTag, ClassTag}
import scala.meta.internal.{ast => m}
import scala.meta.taxonomic.{Context => TaxonomicContext}
import scala.tools.asm._
import scala.meta.internal.tasty._
import scala.meta.internal.ast.mergeTrees
import org.apache.ivy.plugins.resolver._
import org.scalameta.contexts._
import org.scalameta.invariants._

// NOTE: I've been thinking a lot whether I can put this class here, effectively proclaiming
// that classpath-based artifacts, possibly hosted via maven, are the standard.
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
// 2) Taxonomic contexts are removed, and Artifact becomes a plain data structure that carries around
// everything that might be needed from it (path to binaries, sequence of sources, etc).
// With enough pressure, it is possible to even make such artifacts lazy, but then the data model
// becomes so bland that it's just stupid - there's no way to create different kinds of artifacts,
// there's no way to encapsulate anything, there's even no way to create custom platform-dependent tostrings!
//
// All in all, I was left really unsatisfied after all these attempts to abstract away something
// that's hasn't even materialized yet. Therefore, I've decided to hardcode the JVM-based reality for now
// and deal with the possible future once it actually happens.

@context(translateExceptions = true) case class Taxonomy(resolvers: DependencyResolver*) extends TaxonomicContext {
  private case class ResolvedArtifact(binaries: Seq[Path], sources: Seq[Source], resources: Seq[Resource], deps: Seq[Artifact])
  private val cache = mutable.Map[Artifact, ResolvedArtifact]()

  private def resolveUnmanaged(artifact: Artifact.Unmanaged): ResolvedArtifact = cache.getOrElseUpdate(artifact, {
    def failResolve(message: String, ex: Option[Throwable] = None) = throw new ArtifactException(artifact, message, ex)
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
    implicit val dialect = artifact.dialect
    val binaries = artifact.binpath.paths.toList
    val sources = {
      val binfiles = artifact.binpath.explode.filter(x => x._2.toString.endsWith(".class") && !x._2.toString.contains("$"))
      val sourcefiles = artifact.sourcepath.explode.filter(_._2.toString.endsWith(".scala"))
      if (sys.props("tasty.debug") != null) {
        println(binfiles)
        println(sourcefiles)
      }
      sourcefiles.map({ case (_, sourceuri) =>
        // NOTE: sy- and se- prefixes mean the same as in MergeTrees.scala, namely "syntactic" and "semantic".
        val charset = Charset.forName("UTF-8")
        val sourcefile = new File(sourceuri.getPath)
        val content = scala.io.Source.fromFile(sourcefile)(scala.io.Codec(charset)).mkString
        val sha1 = MessageDigest.getInstance("SHA-1")
        sha1.reset()
        sha1.update(content.getBytes("UTF-8"))
        val sydigest = sha1.digest().map(b => "%2X".format(b)).mkString

        val sysource = sourcefile.parse[Source].require[m.Source]
        def toplevelClasses(tree: Tree): List[String] = {
          def loop(prefix: String, tree: Tree): List[String] = tree match {
            case m.Source(stats) => stats.flatMap(stat => loop(prefix, stat)).toList
            case m.Pkg(ref, stats) => stats.flatMap(stat => loop(prefix + "." + ref.toString.replace(".", "/"), stat)).toList
            case m.Defn.Class(_, m.Type.Name(value), _, _, _) => List(prefix + "." + value)
            case m.Defn.Trait(_, m.Type.Name(value), _, _, _) => List(prefix + "." + value)
            case m.Defn.Object(_, m.Term.Name(value), _, _) => List(prefix + "." + value + "$")
            case m.Pkg.Object(_, m.Term.Name(value), _, _) => List(prefix + "." + value + "/package$")
            case _ => Nil
          }
          loop("", tree).map(_.stripPrefix("."))
        }
        val expectedrelatives = toplevelClasses(sysource).map(_.replace(".", "/") + ".class")
        val binuris = expectedrelatives.flatMap(binfiles.get)
        val sesources = binuris.flatMap(binuri => {
          val conn = binuri.toURL.openConnection
          val in = conn.getInputStream
          try {
            var sesource: m.Source = null
            val classReader = new ClassReader(in)
            classReader.accept(new ClassVisitor(Opcodes.ASM4) {
              override def visitAttribute(attr: Attribute) {
                if (attr.`type` == "TASTY") {
                  val valueField = attr.getClass.getDeclaredField("value")
                  valueField.setAccessible(true)
                  val tastyBlob = valueField.get(attr).asInstanceOf[Array[Byte]]
                  val (SyntacticDigest(tastyDialect, tastyDigest), tastySource) = {
                    try Source.fromTasty(tastyBlob)
                    catch { case ex: UntastyException => failResolve(s"deserialization of TASTY from $binuri was unsuccessful", Some(ex)) }
                  }
                  if (dialect != tastyDialect) failResolve("dialects of $sourceuri ($dialect) and $binuri ($tdialect) are different")
                  if (sydigest != tastyDigest) failResolve("source digests of $sourceuri and $binuri are different")
                  sesource = tastySource.require[m.Source]
                }
                super.visitAttribute(attr)
              }
            }, 0)
            if (sesource != null) Some(sesource) else None
          } finally {
            in.close()
          }
        })

        def failCorrelate(message: String) = failResolve(s"$message when correlating $sourceuri and $binuris")
        val systats = sysource.stats
        val sestats = sesources.map(_.stats).flatten
        var matches = mutable.AnyRefMap[Tree, Tree]()
        val mestats = {
          def correlate(sy: m.Stat, ses: Seq[m.Stat]): m.Stat = {
            def correlatePackage(sy: m.Pkg, ses: Seq[m.Stat]): m.Pkg = {
              val ses1 = ses.collect{ case se: m.Pkg if sy.ref.toString == se.ref.toString => se }
              ses1.foreach(se1 => matches(se1) = null)
              val mestats = sy.stats.map(sy => correlate(sy, ses1.flatMap(se => se.stats)))
              sy.copy(stats = mestats)
            }
            def correlateLeaf[T <: m.Member : ClassTag](sy: T, name: m.Name, ses: Seq[m.Stat], fn: PartialFunction[Tree, Boolean]): T = {
              val ses1 = ses.collect{ case tree if fn.lift(tree).getOrElse(false) => tree }
              ses1.foreach(se1 => matches(se1) = null)
              def message(adjective: String) = s"$adjective syntactic ${sy.productPrefix} named $name was found"
              if (ses1.isEmpty) failCorrelate(message("undermatched"))
              else if (ses1.length > 1) failCorrelate(message("overmatched"))
              else mergeTrees(sy, ses1.head)
            }
            sy match {
              case sy: m.Pkg => correlatePackage(sy, ses)
              case sy: m.Defn.Class => correlateLeaf(sy, sy.name, ses, { case se: m.Defn.Class => sy.name.toString == se.name.toString })
              case sy: m.Defn.Trait => correlateLeaf(sy, sy.name, ses, { case se: m.Defn.Trait => sy.name.toString == se.name.toString })
              case sy: m.Defn.Object => correlateLeaf(sy, sy.name, ses, { case se: m.Defn.Object => sy.name.toString == se.name.toString })
              case sy: m.Pkg.Object => correlateLeaf(sy, sy.name, ses, { case se: m.Pkg.Object => sy.name.toString == se.name.toString })
            }
          }
          systats.map(sy => correlate(sy, sestats))
        }
        if (matches.size < sestats.length) failCorrelate("undermatched semantic definitions were found")
        sysource.copy(stats = mestats)
      }).toList
    }
    val resources = Nil // TODO: we can definitely do better, e.g. by indexing all non-class files
    val deps = { // TODO: better heuristic?
      if (artifact.sourcepath.paths.isEmpty) Nil
      else artifact.binpath.paths.map(p => Artifact(Multipath(p), "")).toList
    }
    ResolvedArtifact(binaries, sources, resources, deps)
  })

  private def resolveMaven(artifact: Artifact.Maven): ResolvedArtifact = cache.getOrElseUpdate(artifact, {
    // NOTE: Parts of this file are originally taken from lihaoyi/ammonite:
    // https://github.com/lihaoyi/Ammonite/blob/cd5de73b5601735093f4f80a775423b7a0102b37/repl/src/main/scala/ammonite/repl/IvyThing.scala
    ???
  })

  def binaries(artifact: Artifact): Seq[Path] = artifact match {
    case Artifact.Adhoc(_, _, _) =>
      Nil
    case artifact: Artifact.Unmanaged =>
      resolveUnmanaged(artifact).binaries
    case artifact: Artifact.Maven =>
      resolveMaven(artifact).binaries
  }

  def sources(artifact: Artifact): Seq[Source] = artifact match {
    case Artifact.Adhoc(sources, _, _) =>
      sources
    case artifact: Artifact.Unmanaged =>
      resolveUnmanaged(artifact).sources
    case artifact: Artifact.Maven =>
      resolveMaven(artifact).sources
  }

  def resources(artifact: Artifact): Seq[Resource] = artifact match {
    case Artifact.Adhoc(_, resources, _) =>
      resources
    case artifact: Artifact.Unmanaged =>
      resolveUnmanaged(artifact).resources
    case artifact: Artifact.Maven =>
      resolveMaven(artifact).resources
  }

  def deps(artifact: Artifact): Seq[Artifact] = artifact match {
    case Artifact.Adhoc(_, _, deps) =>
      deps
    case artifact: Artifact.Unmanaged =>
      resolveUnmanaged(artifact).deps
    case artifact: Artifact.Maven =>
      resolveMaven(artifact).deps
  }
}