package scala.meta.tests.vfs

import scala.meta.internal.semanticdb.scalac.SemanticdbPaths._
import scala.meta.internal.semanticdb.scalac.{FileFilter, RemoveOrphanSemanticdbFiles,
  SemanticdbConfig}
import scala.meta.io.{AbsolutePath, RelativePath}

import java.nio.file.{Files, Paths}

import munit.FunSuite

class RemoveOrphanSemantidbFilesSuite extends FunSuite {

  test("orphan files are removed") {
    val sourceroot = Files.createTempDirectory("scalameta")
    val targetroot = Files.createTempDirectory("scalameta")
    val config = SemanticdbConfig.default
      .copy(sourceroot = AbsolutePath(sourceroot), targetroot = AbsolutePath(targetroot))

    val hello = Paths.get("src").resolve("Hello.scala")

    val helloSource = sourceroot.resolve(hello)
    Files.createDirectories(helloSource.getParent)
    Files.createFile(helloSource)

    val helloSemanticdb = toSemanticdb(RelativePath(hello), AbsolutePath(targetroot))
    Files.createDirectories(helloSemanticdb.toNIO.getParent)
    Files.createFile(helloSemanticdb.toNIO)

    val directoryToRemove = helloSemanticdb.toNIO.getParent.resolve("remove")
    val toRemove = directoryToRemove.resolve("Goodbye.scala.semanticdb")
    Files.createDirectories(directoryToRemove)
    Files.createFile(toRemove)

    RemoveOrphanSemanticdbFiles.process(config)

    assert(!Files.isRegularFile(toRemove))
    assert(Files.isRegularFile(helloSemanticdb.toNIO))
  }

  test("excluded files are removed") {
    val sourceroot = Files.createTempDirectory("scalameta")
    val targetroot = Files.createTempDirectory("scalameta")
    val config = SemanticdbConfig.default.copy(
      sourceroot = AbsolutePath(sourceroot),
      targetroot = AbsolutePath(targetroot),
      fileFilter = FileFilter(".*", "Hello")
    )

    val hello = Paths.get("src").resolve("Hello.scala")

    val helloSource = sourceroot.resolve(hello)
    Files.createDirectories(helloSource.getParent)
    Files.createFile(helloSource)

    val helloSemanticdb = toSemanticdb(RelativePath(hello), AbsolutePath(targetroot))
    Files.createDirectories(helloSemanticdb.toNIO.getParent)
    Files.createFile(helloSemanticdb.toNIO)

    RemoveOrphanSemanticdbFiles.process(config)

    assert(!Files.isRegularFile(helloSemanticdb.toNIO))
  }

}
