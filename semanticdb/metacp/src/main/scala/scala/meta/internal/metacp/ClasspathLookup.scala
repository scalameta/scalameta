package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar.JarFile
import scala.collection.mutable
import scala.meta.internal.metacp
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.internal.io.PathIO

final class ClasspathLookup private (val dirs: collection.Map[String, ClasspathEntry.Package]) {

  override def toString: String = {
    s"ClasspathLookup(${dirs.size} entries)"
  }

  def getEntry(path: String): Option[ReadableClasspathEntry] = {
    if (path.endsWith("/")) None
    else {
      val dir = PathIO.dirname(path)
      dirs.get(dir) match {
        case Some(pkg) =>
          val name = PathIO.basename(path)
          pkg.members.get(name).collect {
            case e: ReadableClasspathEntry => e
          }
        case _ =>
          None
      }
    }
  }

  def isPackage(name: String): Boolean =
    dirs.contains(name)

}

object ClasspathLookup {
  case class Error(msg: String) extends Exception(msg)
  def empty: ClasspathLookup = ClasspathLookup(Classpath(Nil))
  def apply(classpath: Classpath): ClasspathLookup = new Builder(classpath).result()

  private final class Builder(classpath: Classpath) {
    private val dirs = mutable.Map.empty[String, ClasspathEntry.Package]

    def result(): ClasspathLookup = {
      val root = ClasspathEntry.Package("/")
      dirs(root.name) = root
      classpath.entries.foreach(expandPath)
      new ClasspathLookup(dirs)
    }

    private def expandPath(path: AbsolutePath): Unit = {
      if (path.isFile) expandJar(path)
      else if (path.isDirectory) expandDirectory(path)
      else throw Error(s"file does not exist: $path")
    }

    private def getPackage(name: String): ClasspathEntry.Package = {
      dirs.get(name) match {
        case Some(dir) =>
          dir
        case _ =>
          val parent = getPackage(PathIO.dirname(name))
          val entry = ClasspathEntry.Package(name)
          parent.members(PathIO.basename(name)) = entry
          dirs(name) = entry
          entry
      }
    }

    private def expandJar(jarpath: AbsolutePath): Unit = {
      val file = jarpath.toFile
      val jar = new JarFile(file)
      try {
        val entries = jar.entries()
        while (entries.hasMoreElements) {
          val entry = entries.nextElement()
          if (!entry.getName.startsWith("META-INF")) {
            val parent = getPackage(
              if (entry.isDirectory) entry.getName
              else PathIO.dirname(entry.getName)
            )
            val inJar = ClasspathEntry.InJar(entry, file)
            parent.members(PathIO.basename(entry.getName)) = inJar
          }
        }
        val manifest = jar.getManifest
        if (manifest != null) {
          val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
          if (classpathAttr != null) {
            classpathAttr.split(" ").foreach { relpath =>
              val abspath = AbsolutePath(jarpath.toNIO.getParent).resolve(relpath)
              expandPath(abspath)
            }
          }
        }
      } finally {
        jar.close()
      }
    }

    private def expandDirectory(root: AbsolutePath): Unit = {
      def relpath(dir: Path): String =
        root.toNIO.relativize(dir).toString + "/"
      Files.walkFileTree(root.toNIO, new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          val name = file.getFileName.toString
          if (name.endsWith(".class")) {
            val dir = dirs(relpath(file.getParent))
            dir.members(name) = ClasspathEntry.OnDisk(AbsolutePath(file))
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
            dirs(name) = metacp.ClasspathEntry.Package(name)
            super.preVisitDirectory(dir, attrs)
          }
        }
      })
    }
  }
}
