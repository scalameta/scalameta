package scala.meta.internal.io

import scala.meta.io.{AbsolutePath, Classpath}

import java.nio.file.{FileVisitor, Files, Path}
import java.util.jar.Manifest

import scala.io.Codec

object Implicits {

  implicit class XtensionClasspath(private val cp: Classpath) extends AnyVal {
    def visit(getVisitor: AbsolutePath => FileVisitor[Path]): Unit = cp.foreach { root =>
      val visitor = getVisitor(root.path)
      Files.walkFileTree(root.path.toNIO, visitor)
    }

    def foreach(fn: ClasspathFile => Unit): Unit = {
      def processEntry(base: AbsolutePath, manifestJar: Option[ClasspathFile]): Unit =
        if (base.isDirectory) fn(ClasspathFile(base, None, manifestJar))
        else if (base.isFile) PathIO.fileExtension(base.toNIO) match {
          case ".jar" => FileIO.withJarFileSystem(base, create = false, close = true) { root =>
              val classpathFile =
                ClasspathFile(root, enclosingJar = Some(base), enclosingManifestJar = manifestJar)
              fn(classpathFile)
              root.toManifest.foreach { manifest =>
                val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
                if (classpathAttr != null) classpathAttr.split(" ").foreach { relativePath =>
                  val jar = base.resolveSibling(_ => relativePath)
                  processEntry(jar, manifestJar = Some(classpathFile))
                }
              }
            }
          case _ => sys.error(s"Expected jar file, obtained $base")
        }
        else ()
      cp.entries.foreach(entry => processEntry(entry, manifestJar = None))
    }
  }

  implicit class XtensionAbsolutePathManifest(private val root: AbsolutePath) extends AnyVal {
    def toManifest: Option[Manifest] = {
      val manifestPath = root.resolve("META-INF/MANIFEST.MF")
      if (manifestPath.isFile) {
        val in = Files.newInputStream(manifestPath.toNIO)
        try Some(new Manifest(in))
        finally in.close()
      } else None
    }
  }

  implicit class XtensionNioPath(private val path: Path) extends AnyVal {

    def exists: Boolean = FileIO.exists(path)
    def isDirectory: Boolean = FileIO.isDirectory(path)
    def isRegularFile: Boolean = FileIO.isRegularFile(path)
    def isRegularFileNoLinks: Boolean = FileIO.isRegularFileNoLinks(path)
    def findFiles: Seq[Path] = FileIO.findFiles(path)
    def readFile(implicit codec: Codec): String = FileIO.readFile(path)
    def writeFile(content: String)(implicit codec: Codec): Unit = FileIO.writeFile(path, content)
    def delete(): Unit = FileIO.delete(path)
    def mkdir(): Unit = FileIO.mkdir(path)
    def mkdirs(): Unit = FileIO.mkdirs(path)

  }

}
