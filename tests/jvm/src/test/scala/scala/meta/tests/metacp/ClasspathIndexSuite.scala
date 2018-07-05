package scala.meta.tests.metacp

import org.scalatest.FunSuite
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.tests.BuildInfo
import scala.meta.tests.semanticdb.ManifestMetacp

class ClasspathIndexSuite extends FunSuite {
  def classpath(path: AbsolutePath) =
    Classpath(
      path ::
        Library.scalaLibrary.classpath().entries ++
        Library.jdk.classpath().entries
    )

  test("manifest") {
    val jar = AbsolutePath(ManifestMetacp.path.getParent.resolve("manifest.jar"))
    val manifest = ClasspathIndex(classpath(jar))
    assert(manifest.getClassfile("A.class").isDefined)
  }

  test("classpath") {
    val dir = AbsolutePath(BuildInfo.databaseClasspath)
    val index = ClasspathIndex(classpath(dir))

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

}
