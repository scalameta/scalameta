package scala.meta
package artifacts

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
import scala.tools.asm._
import scala.meta.internal.tasty._
import scala.meta.internal.artifacts._
import scala.meta.parsers._
import org.apache.ivy.plugins.resolver._
import org.scalameta._
import org.scalameta.invariants._

// NOTE: I've been thinking a lot whether I can put this class here, effectively proclaiming
// that classpath-based artifacts, possibly hosted via maven, are the standard.
// It is the standard now, but obviously once we have new and exciting language dialects
// or maybe additional backends, the situation may change.
//
// Over the last week, I've made a few design attempts that involved abstracting
// all kinds of details that may be deemed platform-specific. I'll mention two prominent results here.
//
// 1) Artifact contexts are implemented in hosts, and every host requires an artifact context to
// instantiate a platform-dependent semantic context. All was fine until I realized that one may
// erroneously mix artifact and semantic contexts from different platforms, which will lead to fail.
//
// 2) Artifact contexts are removed, and Artifact becomes a plain data structure that carries around
// everything that might be needed from it (path to binaries, sequence of sources, etc).
// With enough pressure, it is possible to even make such artifacts lazy, but then the data model
// becomes so bland that it's just stupid - there's no way to create different kinds of artifacts,
// there's no way to encapsulate anything, there's even no way to create custom platform-dependent tostrings!
//
// All in all, I was left really unsatisfied after all these attempts to abstract away something
// that's hasn't even materialized yet. Therefore, I've decided to hardcode the JVM-based reality for now
// and deal with the possible future once it actually happens.

case class Ecosystem(resolvers: DependencyResolver*) extends Resolver {
  private case class ResolvedArtifact(binaries: Seq[Path], sources: Seq[Source], resources: Seq[Resource], deps: Seq[Artifact])
  private val cache = mutable.Map[Artifact, ResolvedArtifact]()

  private def resolveUnmanaged(artifact: Artifact.Unmanaged): ResolvedArtifact = cache.getOrElseUpdate(artifact, {
    def failResolve(message: String, ex: Option[Throwable] = None) = throw new ArtifactException(artifact, message, ex)
    try {
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
      logArtifact(println(s"resolving $artifact"))
      val binaries: Seq[Path] = artifact.binpath.paths.toList
      val sources: Seq[Source] = {
        def loadTasty(uri: URI): Option[Source] = {
          val conn = uri.toURL.openConnection
          val in = conn.getInputStream
          try {
            var source: Source = null
            val classReader = new ClassReader(in)
            classReader.accept(new ClassVisitor(Opcodes.ASM4) {
              override def visitAttribute(attr: Attribute) {
                if (attr.`type` == "TASTY") {
                  logArtifact(println(s"found TASTY section in $uri"))
                  val valueField = attr.getClass.getDeclaredField("value")
                  valueField.setAccessible(true)
                  val tastyBlob = valueField.get(attr).asInstanceOf[Array[Byte]]
                  val tastySource = {
                    try fromTasty(tastyBlob)
                    catch { case ex: UntastyException => failResolve(s"deserialization of TASTY from $uri was unsuccessful", Some(ex)) }
                  }
                  source = tastySource
                }
                super.visitAttribute(attr)
              }
            }, 0)
            if (source != null) Some(source) else None
          } finally {
            in.close()
          }
        }

        // TODO: Find ways to quickly check that a given binfile has a TASTY section.
        // We could probably require a manifest entry in a directory/JAR or something,
        // but I don't have time to think this over right now.
        var binfiles = artifact.binpath.explode.filter(_._2.toString.endsWith(".class"))
        binfiles = binfiles.filter(!_._2.toString.contains("$")) // NOTE: exclude inner classes
        binfiles = binfiles.filter(!_._2.toString.matches(""".*scala-library(-\d+\.\d+\.\d+)?.jar!.*""")) // NOTE: exclude stdlib
        binfiles.flatMap({ case (relativepath, binuri) =>
          logTasty(println(s"considering binfile at $binuri ($relativepath)"))
          loadTasty(binuri)
        }).toList
      }
      val resources: Seq[Resource] = Nil // TODO: we can definitely do better, e.g. by indexing all non-class files
      val deps: Seq[Artifact] = Nil // TODO: we should improve this
      ResolvedArtifact(binaries, sources, resources, deps)
    } catch {
      case ex: ArtifactException => throw ex
      case other: Exception => failResolve(other.getMessage, Some(other))
    }
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