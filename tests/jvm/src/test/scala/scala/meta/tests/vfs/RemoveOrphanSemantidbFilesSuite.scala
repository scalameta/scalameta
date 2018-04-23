package scala.meta.tests.vfs

import java.nio.file.Files
import java.nio.file.Paths
import org.scalatest.FunSuite
import org.langmeta.io.AbsolutePath
import org.langmeta.io.RelativePath
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths._
import scala.meta.internal.semanticdb.scalac.RemoveOrphanSemanticdbFiles

class RemoveOrphanSemantidbFilesSuite extends FunSuite {

  test("orphan files are removed") {
    val sourceroot = Files.createTempDirectory("scalameta")
    val targetroot = Files.createTempDirectory("scalameta")

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

    RemoveOrphanSemanticdbFiles.process(AbsolutePath(sourceroot), AbsolutePath(targetroot))

    assert(!Files.isRegularFile(toRemove))
    assert(Files.isRegularFile(helloSemanticdb.toNIO))
  }

}
