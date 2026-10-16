package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb._

import java.nio.file._
import java.nio.file.attribute.PosixFilePermissions
import java.util.jar._
import java.util.zip._

import scala.util.Try

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1604
class Issue1604Suite extends FunSuite {

  private val payload = TextDocuments(Seq(TextDocument(uri = "A.scala"))).toByteArray

  private val dir = FunFixture[Path](_ => Files.createTempDirectory("scalameta-1604"), delete)

  private def delete(path: Path): Unit = {
    if (!Files.isSymbolicLink(path)) {
      Try(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwx------")))
      Option(path.toFile.listFiles()).foreach(_.foreach(x => delete(x.toPath)))
    }
    Files.delete(path)
  }

  private def assumePosix(): Unit = assume(
    FileSystems.getDefault.supportedFileAttributeViews.contains("posix"),
    "POSIX file permissions not supported on this platform",
  )

  private def unreadable(path: Path): Path = {
    assumePosix()
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("---------"))
    // running as root bypasses permission checks
    assume(!Files.isReadable(path), s"$path still readable (running as root?)")
    path
  }

  private def write(path: Path, bytes: Array[Byte]): Path = {
    Files.createDirectories(path.getParent)
    Files.write(path, bytes)
  }

  private def jar(path: Path, classPath: Option[String] = None)(entries: String*): Path = {
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    classPath.foreach(manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, _))
    val os = new JarOutputStream(Files.newOutputStream(path), manifest)
    try entries.foreach { name =>
        os.putNextEntry(new JarEntry(name))
        os.write(payload)
        os.closeEntry()
      }
    finally os.close()
    path
  }

  /** Lists each visited path with its documents or parse error, then what Locator throws. */
  private def locate(root: Path, paths: Path*): List[String] = {
    val out = List.newBuilder[String]
    def show(path: Path) = (if (path.startsWith(root)) root.relativize(path) else path).toString
      .replace('\\', '/')
    try Locator(paths.toList) { (path, docs) =>
        val res = Try(docs()).fold(_.getClass.getSimpleName, _.documents.map(_.uri).mkString(","))
        out += s"${show(path)}: $res"
      }
    catch { case e: Throwable => out += s"throws ${e.getClass.getSimpleName}" }
    out.result()
  }

  dir.test("missing path")(root =>
    assertEquals(locate(root, root.resolve("missing.jar"), root.resolve("missing")), Nil),
  )

  dir.test("directory") { root =>
    write(root.resolve("sub/b.semanticdb"), payload)
    write(root.resolve("a.semanticdb"), payload)
    assertEquals(locate(root, root), List("a.semanticdb: A.scala", "sub/b.semanticdb: A.scala"))
  }

  dir.test("jar") { root =>
    val path = jar(root.resolve("a.jar"))("META-INF/a.semanticdb")
    assertEquals(locate(root, path), List("META-INF/a.semanticdb: A.scala"))
  }

  dir.test("non-zip file named .jar") { root =>
    val path = write(root.resolve("a.jar"), "not a zip".getBytes)
    write(root.resolve("b.semanticdb"), payload)
    assertEquals(locate(root, path, root), List("b.semanticdb: A.scala"))
  }

  dir.test("empty file named .jar") { root =>
    val path = write(root.resolve("a.jar"), Array.emptyByteArray)
    write(root.resolve("b.semanticdb"), payload)
    assertEquals(locate(root, path, root), List("b.semanticdb: A.scala"))
  }

  dir.test("directory named .jar") { root =>
    val path = root.resolve("a.jar")
    write(path.resolve("a.semanticdb"), payload)
    assertEquals(locate(root, path), List("a.jar/a.semanticdb: A.scala"))
  }

  dir.test("unreadable jar") { root =>
    val path = unreadable(jar(root.resolve("a.jar"))("META-INF/a.semanticdb"))
    write(root.resolve("b.semanticdb"), payload)
    assertEquals(locate(root, path, root), List("b.semanticdb: A.scala"))
  }

  dir.test("unreadable .semanticdb file") { root =>
    val path = unreadable(write(root.resolve("a.semanticdb"), payload))
    assertEquals(locate(root, path), List("throws AccessDeniedException"))
  }

  dir.test("unreadable .semanticdb file in a directory") { root =>
    unreadable(write(root.resolve("a.semanticdb"), payload))
    write(root.resolve("b.semanticdb"), payload)
    assertEquals(locate(root, root), List("throws AccessDeniedException"))
  }

  dir.test("unreadable directory") { root =>
    val path = unreadable(Files.createDirectory(root.resolve("sub")))
    write(root.resolve("a.semanticdb"), payload)
    assertEquals(locate(root, path, root), List("a.semanticdb: A.scala"))
  }

  dir.test("symlink loop in a directory") { root =>
    assumePosix()
    write(root.resolve("a.semanticdb"), payload)
    Files.createSymbolicLink(root.resolve("loop"), root)
    assertEquals(locate(root, root), List("a.semanticdb: A.scala"))
  }

  dir.test("symlink cycle") { root =>
    assumePosix()
    Files.createSymbolicLink(root.resolve("a.semanticdb"), root.resolve("b.semanticdb"))
    Files.createSymbolicLink(root.resolve("b.semanticdb"), root.resolve("a.semanticdb"))
    assertEquals(locate(root, root.resolve("a.semanticdb"), root), Nil)
  }

  dir.test("truncated .semanticdb in a jar") { root =>
    val path = root.resolve("a.jar")
    val os = new JarOutputStream(Files.newOutputStream(path))
    try {
      os.putNextEntry(new JarEntry("a.semanticdb"))
      os.write(payload, 0, payload.length - 1)
      os.closeEntry()
    } finally os.close()
    assertEquals(locate(root, path), List("a.semanticdb: InvalidProtocolBufferException"))
  }

  dir.test("corrupt local header in a jar") { root =>
    val path = jar(root.resolve("a.jar"))("a.semanticdb", "b.semanticdb")
    val bytes = Files.readAllBytes(path)
    // the local header comes first and ends with the name, after 30 bytes of fields
    bytes(new String(bytes, "ISO-8859-1").indexOf("a.semanticdb") - 30) = 0
    Files.write(path, bytes)
    assertEquals(locate(root, path), List("a.semanticdb: ZipException", "b.semanticdb: A.scala"))
  }

  dir.test("corrupt manifest in a jar") { root =>
    val path = root.resolve("a.jar")
    val os = new ZipOutputStream(Files.newOutputStream(path))
    try {
      os.putNextEntry(new ZipEntry(JarFile.MANIFEST_NAME))
      os.write("no colon\n".getBytes)
      os.closeEntry()
      os.putNextEntry(new ZipEntry("a.semanticdb"))
      os.write(payload)
      os.closeEntry()
    } finally os.close()
    assertEquals(locate(root, path), List("a.semanticdb: A.scala"))
  }

  dir.test("jar with Class-Path") { root =>
    jar(root.resolve("b.jar"))("b.semanticdb")
    val path = jar(root.resolve("a.jar"), Some("missing.jar b.jar"))("a.semanticdb")
    assertEquals(locate(root, path), List("a.semanticdb: A.scala", "b.semanticdb: A.scala"))
  }

  dir.test("jar with Class-Path naming itself") { root =>
    val path = jar(root.resolve("a.jar"), Some("a.jar"))("a.semanticdb")
    assertEquals(
      locate(root, path).distinct,
      List("a.semanticdb: A.scala", "throws StackOverflowError"),
    )
  }

  dir.test("jars whose Class-Path names each other") { root =>
    jar(root.resolve("b.jar"), Some("a.jar"))("b.semanticdb")
    val path = jar(root.resolve("a.jar"), Some("b.jar"))("a.semanticdb")
    assertEquals(
      locate(root, path).distinct,
      List("a.semanticdb: A.scala", "b.semanticdb: A.scala", "throws StackOverflowError"),
    )
  }

  dir.test("jar with Class-Path separated by two spaces") { root =>
    jar(root.resolve("b.jar"))("b.semanticdb")
    write(root.resolve("c.semanticdb"), payload)
    val path = jar(root.resolve("a.jar"), Some("b.jar  b.jar"))("a.semanticdb")
    assertEquals(
      locate(root, path),
      List(
        "a.semanticdb: A.scala",
        "b.semanticdb: A.scala",
        "c.semanticdb: A.scala",
        "b.semanticdb: A.scala",
      ),
    )
  }

}
