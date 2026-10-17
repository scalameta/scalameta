package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath

import java.nio.file.{Files, Path}

import munit.FunSuite

class Issue4933Suite extends FunSuite {

  private val doc = s.TextDocument(uri = "a/A.scala", text = "object A")
  private val diag = s
    .TextDocument(uri = "a/A.scala", diagnostics = Seq(s.Diagnostic(message = "warning")))
  private val cached = "cached".getBytes.toSeq

  private def bytes(path: Path): Seq[Byte] = Files.readAllBytes(path).toSeq
  private def bytes(docs: s.TextDocument*): Seq[Byte] = s.TextDocuments(docs).toByteArray.toSeq

  private def setup(): (AbsolutePath, Path) = {
    val root = AbsolutePath(Files.createTempDirectory("issue4933_"))
    val out = SemanticdbPaths.toSemanticdb(doc, root).toNIO
    Files.createDirectories(out.getParent)
    (root, out)
  }

  private def link(root: AbsolutePath, out: Path): Path = {
    val blob = Files.write(root.toNIO.resolve("blob"), cached.toArray)
    Files.createSymbolicLink(out, blob)
    blob
  }

  test("save over a symlink") {
    val (root, out) = setup()
    val blob = link(root, out)
    doc.save(root)
    assert(!Files.isSymbolicLink(out))
    assertEquals(bytes(blob), cached)
    assertEquals(bytes(out), bytes(doc))
  }

  test("save and append over a symlink") {
    val (root, out) = setup()
    val blob = link(root, out)
    doc.save(root)
    diag.append(root)
    assert(!Files.isSymbolicLink(out))
    assertEquals(bytes(blob), cached)
    assertEquals(bytes(out), bytes(doc, diag))
  }

  test("save and append to a new file") {
    val (root, out) = setup()
    doc.save(root)
    diag.append(root)
    assertEquals(bytes(out), bytes(doc, diag))
  }

  test("append to a new file") {
    val (root, out) = setup()
    diag.append(root)
    assertEquals(bytes(out), bytes(diag))
  }

  test("save with a temp file left by a killed run") {
    val (root, out) = setup()
    val tmp = out.resolveSibling(out.getFileName.toString + ".tmp")
    Files.write(tmp, cached.toArray)
    doc.save(root)
    assertEquals(bytes(out), bytes(doc))
    assertEquals(Files.exists(tmp), false)
  }

  test("append to a corrupt file") {
    val (root, out) = setup()
    Files.write(out, cached.toArray)
    diag.append(root)
    assertEquals(bytes(out), bytes(diag))
  }

}
