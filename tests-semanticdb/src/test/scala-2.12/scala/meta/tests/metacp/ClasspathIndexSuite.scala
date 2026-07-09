package scala.meta.tests.metacp

import scala.meta.cli.Reporter
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.io.PathIO
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.tests.BuildInfo
import scala.meta.tests.semanticdb.ManifestMetacp

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.Paths

import munit.FunSuite

class ClasspathIndexSuite extends FunSuite {
  def classpath(path: AbsolutePath) = Classpath(path :: Library.scalaLibrary.classpath().entries)

  test("manifest") {
    val jar = AbsolutePath(ManifestMetacp.path.getParent.resolve("manifest.jar"))
    val manifest = ClasspathIndex(classpath(jar))
    assert(manifest.getClassfile("A.class").isDefined)
  }

  test("classpath") {
    val dir = AbsolutePath(BuildInfo.databaseClasspath)
    val index = ClasspathIndex(classpath(dir), includeJdk = true)

    // JDK
    assert(index.isClassdir("org/xml/"))
    assert(index.isClassdir("org/xml/sax/"))
    assert(index.isClassdir("javax/net/"))
    assert(index.isClassdir("java/util/"))

    // scala-library
    assert(index.isClassdir("scala/"))
    assert(index.isClassdir("scala/util/"))
    assert(index.isClassdir("scala/collection/"))
    assert(index.getClassfile("scala/Predef$.class").isDefined)
    assert(index.getClassfile("scala/collection/$colon$plus.class").isDefined)

    // semanticdb/integration
    assert(index.isClassdir("com/javacp/"))
    assert(index.isClassdir("flags/"))
    assert(index.isClassdir("flags/p/"))
    assert(index.isClassdir("advanced/"))

    assert(index.getClassfile("com/javacp/MetacJava.class").isDefined)
    assert(index.getClassfile("com/javacp/MetacJava$StaticInner.class").isDefined)
    assert(index.getClassfile("flags/p/package$AA.class").isDefined)
  }

  test("ignore non-existent") {
    val classpath = Classpath(PathIO.workingDirectory.resolve("doesnotexist.jar"))
    val index = ClasspathIndex(classpath)
  }

  test("ignore classpath entry for files that are not jar/zip") {
    val classpath = Classpath(Paths.get(getClass().getResource("/metac.index").toURI()))
    val index = ClasspathIndex(classpath)
  }

  // indexes the given resource jar, returning the index and whatever the
  // reporter emitted on stderr (warnings about cyclic Class-Path references).
  def indexResource(name: String): (ClasspathIndex, String) = {
    val classpath = Classpath(Paths.get(getClass().getResource(name).toURI()))
    val errStream = new ByteArrayOutputStream()
    val reporter = Reporter().withSilentOut().withErr(new PrintStream(errStream))
    val index = ClasspathIndex(classpath, reporter = reporter)
    (index, errStream.toString())
  }

  test("self-referencing jar") {
    // self-ref.jar's manifest lists itself on its Class-Path.
    def load() = indexResource("/self-ref.jar")
    val (index, err) = load()
    assert(index.getClassfile("A.class").isDefined)
    assert(err.startsWith("warning: classpath cycle detected"), err)
    assert(err.endsWith("self-ref.jar -> self-ref.jar\n"), err)
  }

  test("mutually-referencing jars") {
    // cycle-a -> cycle-b -> cycle-c -> cycle-b.
    def load() = indexResource("/cycle-a.jar")
    val (_, err) = load()
    assert(err.startsWith("warning: classpath cycle detected"), err)
    assert(err.endsWith("cycle-b.jar -> cycle-c.jar -> cycle-b.jar\n"), err)
  }

  test("shared (diamond) Class-Path reference") {
    // diamond-a -> {diamond-b, diamond-c}, diamond-b -> diamond-c: diamond-c is
    // shared by two paths but not part of a cycle.
    def load() = indexResource("/diamond-a.jar")
    val (index, err) = load()
    assert(index.getClassfile("A.class").isDefined)
    // wrong: the naive fix mistakes diamond-c's shared visit for a cycle.
    assert(err.startsWith("warning: classpath cycle detected"), err)
    assert(err.endsWith("diamond-c.jar -> diamond-c.jar\n"), err)
  }
}
