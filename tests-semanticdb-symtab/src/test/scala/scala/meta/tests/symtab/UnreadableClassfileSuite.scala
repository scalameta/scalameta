package scala.meta.tests.symtab

import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.tests.BuildInfo

import java.nio.file.{Files, Path, Paths}
import java.util.jar.{JarEntry, JarOutputStream}

import munit.FunSuite

class UnreadableClassfileSuite extends FunSuite {

  /* ASM reads the major version as a signed short, so 0x7fff is the largest
   * version that every ASM release rejects. */
  private val unreadableClassfile: Array[Byte] = {
    val bytes = Files.readAllBytes(Paths.get(BuildInfo.databaseClasspath).resolve("A.class"))
    bytes(6) = 0x7f.toByte
    bytes(7) = 0xff.toByte
    bytes
  }

  private def load(entry: Path): Throwable = {
    val symtab = GlobalSymbolTable(Classpath(AbsolutePath(entry)))
    intercept[IllegalArgumentException](symtab.info("_empty_/A#"))
  }

  test("unreadable classfile in a directory") {
    val dir = Files.createTempDirectory("unreadable_")
    dir.toFile.deleteOnExit()
    Files.write(dir.resolve("A.class"), unreadableClassfile)
    val obtained = load(dir)
    assertNoDiff(
      obtained.getMessage,
      s"Unsupported class file major version 32767 in ${dir.resolve("A.class")}",
    )
  }

  test("unreadable classfile in a jar") {
    val jar = Files.createTempFile("unreadable_", ".jar")
    jar.toFile.deleteOnExit()
    val out = new JarOutputStream(Files.newOutputStream(jar))
    try {
      out.putNextEntry(new JarEntry("A.class"))
      out.write(unreadableClassfile)
      out.closeEntry()
    } finally out.close()
    val obtained = load(jar)
    assertNoDiff(obtained.getMessage, s"Unsupported class file major version 32767 in $jar!/A.class")
  }
}
