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
final class ClasspathIndex private (val dirs: collection.Map[String, Classdir]) {

  override def toString: String = {
    s"ClasspathIndex(${dirs.size} entries)"
  }

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

}

object ClasspathIndex {
  def apply(classpath: Classpath): ClasspathIndex = new Builder(classpath).result()

  private final class Builder(classpath: Classpath) {
    private val dirs = mutable.Map.empty[String, Classdir]

    def result(): ClasspathIndex = {
      val root = Classdir("/")
      dirs(root.name) = root
      classpath.entries.foreach(expandEntry)
      new ClasspathIndex(dirs)
    }

    private def expandEntry(path: AbsolutePath): Unit = {
      if (path.isFile) expandJarEntry(path)
      else if (path.isDirectory) expandDirEntry(path)
      else throw new IllegalArgumentException(path.toString)
    }

    private def getClassdir(name: String): Classdir = {
      dirs.get(name) match {
        case Some(dir) =>
          dir
        case _ =>
          val parent = getClassdir(PathIO.dirname(name))
          val entry = Classdir(name)
          parent.members(PathIO.basename(name)) = entry
          dirs(name) = entry
          entry
      }
    }

    private def expandJarEntry(jarpath: AbsolutePath): Unit = {
      val file = jarpath.toFile
      val jar = new JarFile(file)
      try {
        val entries = jar.entries()
        while (entries.hasMoreElements) {
          val entry = entries.nextElement()
          if (!entry.getName.startsWith("META-INF")) {
            val parent = getClassdir(
              if (entry.isDirectory) entry.getName
              else PathIO.dirname(entry.getName)
            )
            val inJar = CompressedClassfile(entry, file)
            parent.members(PathIO.basename(entry.getName)) = inJar
          }
        }
        val manifest = jar.getManifest
        if (manifest != null) {
          val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
          if (classpathAttr != null) {
            classpathAttr.split(" ").foreach { relpath =>
              val abspath = AbsolutePath(jarpath.toNIO.getParent).resolve(relpath)
              expandEntry(abspath)
            }
          }
        }
      } finally {
        jar.close()
      }
    }

    private def expandDirEntry(root: AbsolutePath): Unit = {
      def relpath(dir: Path): String =
        root.toNIO.relativize(dir).iterator().asScala.mkString("", "/", "/")
      Files.walkFileTree(root.toNIO, new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          val name = file.getFileName.toString
          if (name.endsWith(".class")) {
            val dir = dirs(relpath(file.getParent))
            dir.members(name) = UncompressedClassfile(AbsolutePath(file))
          }
          super.visitFile(file, attrs)
        }
        override def preVisitDirectory(
            dir: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if (dir.endsWith("META-INF")) FileVisitResult.SKIP_SUBTREE
          else {
            val name = relpath(dir)
            dirs(name) = Classdir(name)
            super.preVisitDirectory(dir, attrs)
          }
        }
      })
    }
  }
}
