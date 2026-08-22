package scala.meta.tests
package integrity

import java.io.File
import java.io.File.{pathSeparator, separator}
import java.util.zip.ZipFile

import scala.collection.JavaConverters._

import munit._

class IntegritySuite extends FunSuite {
  // NOTE: Here's a real-life issue that this test suite has detected.
  // Overlapping classfiles:
  // /Users/xeno_by/Projects/core/foundation/target/scala-2.11/classes/scala/meta/internal/ast/registry.class and
  // /Users/xeno_by/Projects/core/scalameta/trees/target/scala-2.11/classes/scala/meta/internal/ast/Registry.class

  test("classfiles don't overlap") {
    def deepfiles(f: File): List[File] = {
      val shallow = f.listFiles.toList
      shallow.filter(_.isFile) ++ shallow.filter(_.isDirectory).flatMap(deepfiles)
    }

    // a classpath entry is a directory of classfiles or a jar of them, depending on the sbt version
    def classfiles(entry: File): List[(String, String)] =
      if (entry.isDirectory) deepfiles(entry).map(_.getAbsolutePath).filter(_.endsWith(".class"))
        .map { abspath =>
          val relpath = abspath.substring(entry.getAbsolutePath.length).stripPrefix(separator)
          (relpath, abspath)
        }
      else if (entry.getName.endsWith(".jar")) {
        val zip = new ZipFile(entry)
        try zip.entries.asScala.map(_.getName).filter(_.endsWith(".class"))
            .map(name => (name.replace("/", separator), s"$entry!$name")).toList
        finally zip.close()
      } else Nil

    val fullcp = sys.props("sbt.paths.tests.test.classes").split(pathSeparator).toList
    val cp = fullcp.filter(_.contains(separator + "target" + separator))
    assert(cp.nonEmpty, "no build outputs on the test classpath")

    var success = true
    val relpaths = scala.collection.mutable.Map[String, String]()
    cp.foreach(dir =>
      classfiles(new File(dir)).foreach { case (relpath0, abspath) =>
        val relpath = relpath0.toLowerCase
        if (relpaths.contains(relpath)) {
          success = false
          Console.err.println(s"Overlapping classfiles: ${relpaths(relpath)} and $abspath")
        } else relpaths(relpath) = abspath
      },
    )

    if (!success) fail("Detected overlapping classfiles")
  }
}
