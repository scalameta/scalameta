package scala.meta.tests.metacp

import org.scalatest.FunSuite
import scala.meta.internal.io.PathIO
import scala.meta.internal.metacp.ClasspathLookup
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.tests.BuildInfo
import scala.meta.tests.semanticdb.ManifestMetacp

class ClasspathLookupSuite extends FunSuite {
  def classpath(path: AbsolutePath) =
    Classpath(
      path ::
        Library.scalaLibrary.classpath().entries ++
        Library.jdk.classpath().entries
    )

  test("manifest") {
    val jar = AbsolutePath(ManifestMetacp.path.getParent.resolve("manifest.jar"))
    val manifest = ClasspathLookup(classpath(jar))
    assert(manifest.getEntry("Test.class").isDefined)
  }

  test("classpath") {
    val dir = AbsolutePath(BuildInfo.databaseClasspath)
    val lookup = ClasspathLookup(classpath(dir))

    // JDK
    assert(lookup.isPackage("org/xml/"))
    assert(lookup.isPackage("org/xml/sax/"))
    assert(lookup.isPackage("javax/net/"))
    assert(lookup.isPackage("java/util/"))

    // scala-library
    assert(lookup.isPackage("scala/"))
    assert(lookup.isPackage("scala/util/"))
    assert(lookup.isPackage("scala/collection/"))
    assert(lookup.getEntry("scala/Predef$.class").isDefined)
    assert(lookup.getEntry("scala/collection/$colon$plus.class").isDefined)

    // semanticdb/integration
    assert(lookup.isPackage("com/javacp/"))
    assert(lookup.isPackage("flags/"))
    assert(lookup.isPackage("flags/p/"))
    assert(lookup.isPackage("advanced/"))

    assert(lookup.getEntry("com/javacp/MetacJava.class").isDefined)
    assert(lookup.getEntry("com/javacp/MetacJava$StaticInner.class").isDefined)
    assert(lookup.getEntry("flags/p/package$AA.class").isDefined)
  }

  test("error") {
    val error = intercept[ClasspathLookup.Error] {
      ClasspathLookup(Classpath(PathIO.workingDirectory.resolve("doesnotexist.jar")))
    }
    assert(error.getMessage.contains(""))
    assert(error.getMessage.contains("doesnotexist.jar"))
  }

}
