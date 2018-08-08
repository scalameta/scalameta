package scala.meta.internal.classpath

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar.JarFile
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.internal.io.PathIO

/** An index to lookup class directories and classfiles by their JVM names. */
final class ClasspathIndex private (
    classpath: Classpath,
    val dirs: collection.Map[String, Classdir]) {

  /** Returns a classfile with the given path. */
  def getClassfile(path: String): Option[Classfile] = {
    getClassfile(PathIO.dirname(path), PathIO.basename(path))
  }

  /** Returns a classfile with the given directory and filename. */
  def getClassfile(directory: String, filename: String): Option[Classfile] = {
    dirs.get(directory) match {
      case Some(pkg) =>
        pkg.members.get(filename).collect {
          case e: Classfile => e
        }
      case _ =>
        None
    }
  }

  /** Returns true if this path is a class directory. */
  def isClassdir(path: String): Boolean =
    dirs.contains(path)

  override def toString: String = {
    s"ClasspathIndex($classpath)"
  }
}

object ClasspathIndex {
  def apply(classpath: Classpath): ClasspathIndex = new Builder(classpath).result()

  private final class Builder(classpath: Classpath) {
    private val dirs = mutable.Map.empty[String, Classdir]

    def result(): ClasspathIndex = {
      val root = Classdir("/")
      dirs(root.relativeUri) = root
      classpath.entries.foreach(expandEntry)
      new ClasspathIndex(classpath, dirs)
    }

    private def expandEntry(path: AbsolutePath): Unit = {
      if (path.isFile) expandJarEntry(path)
      else if (path.isDirectory) expandDirEntry(path)
      else ()
    }

    private def addMember(parent: Classdir, basename: String, element: ClasspathElement): Unit = {
      // First element wins in case of conflicts to match scala-compiler behavior:
      // $ scala -classpath $(coursier fetch org.scalameta:metacp_2.12:4.0.0-M3 -p):$(coursier fetch org.scalameta:metacp_2.12:4.0.0-M4 -p)
      // scala> scala.meta.internal.metacp.BuildInfo
      // res0: meta.internal.metacp.BuildInfo.type = version: 4.0.0-M3
      //
      // $ scala -classpath $(coursier fetch org.scalameta:metacp_2.12:4.0.0-M4 -p):$(coursier fetch org.scalameta:metacp_2.12:4.0.0-M3 -p)
      // scala> scala.meta.internal.metacp.BuildInfo
      // res0: meta.internal.metacp.BuildInfo.type = version: 4.0.0-M4
      if (!parent.members.contains(basename)) {
        parent.members(basename) = element
      }
    }

    private def getClassdir(name: String): Classdir = {
      dirs.get(name) match {
        case Some(dir) =>
          dir
        case _ =>
          val parent = getClassdir(PathIO.dirname(name))
          val element = Classdir(name)
          parent.members(PathIO.basename(name)) = element
          dirs(name) = element
          element
      }
    }

    private def expandJarEntry(jarpath: AbsolutePath): Unit = {
      val file = jarpath.toFile
      val jar = new JarFile(file)
      try {
        val entries = jar.entries()
        while (entries.hasMoreElements) {
          val element = entries.nextElement()
          if (!element.getName.startsWith("META-INF")) {
            val parent = getClassdir(
              if (element.isDirectory) element.getName
              else PathIO.dirname(element.getName)
            )
            val inJar = CompressedClassfile(element, file)
            addMember(parent, PathIO.basename(element.getName), inJar)
          }
        }
        val manifest = jar.getManifest
        if (manifest != null) {
          val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
          if (classpathAttr != null) {
            classpathAttr.split(" ").foreach { relpath =>
              val abspath = AbsolutePath(jarpath.toNIO.getParent).resolve(relpath)
              if (abspath.isFile || abspath.isDirectory) {
                expandEntry(abspath)
              }
            }
          }
        }
      } finally {
        jar.close()
      }
    }

    private def expandDirEntry(root: AbsolutePath): Unit = {
      Files.walkFileTree(root.toNIO, new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          val name = file.getFileName.toString
          if (name.endsWith(".class")) {
            val relpath = AbsolutePath(file).toRelative(root)
            val reluri = relpath.toURI(isDirectory = false).toString
            val basename = PathIO.basename(reluri)
            val dirname = PathIO.dirname(reluri)
            val classdir = getClassdir(dirname)
            val element = UncompressedClassfile(reluri, AbsolutePath(file))
            addMember(classdir, basename, element)
          }
          super.visitFile(file, attrs)
        }
        override def preVisitDirectory(
            dir: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if (dir.endsWith("META-INF")) FileVisitResult.SKIP_SUBTREE
          else FileVisitResult.CONTINUE
        }
      })
    }
  }
}
