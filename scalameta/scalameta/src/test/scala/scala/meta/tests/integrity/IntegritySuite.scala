package scala.meta.tests
package integrity

import org.scalatest._
import java.io.File
import java.io.File.{separator, pathSeparator}

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

    val fullcp = sys.props("sbt.paths.scalameta.test.classes").split(pathSeparator).toList
    val cp = fullcp.filter(_.contains(separator + "target" + separator))

    var success = true
    val relpaths = scala.collection.mutable.Map[String, String]()
    cp.foreach(dir => {
      val classfiles = deepfiles(new File(dir)).map(_.getAbsolutePath).filter(_.endsWith(".class"))
      classfiles.foreach(abspath => {
        var relpath = abspath.substring(dir.length)
        if (relpath.startsWith(separator)) relpath = relpath.substring(1)
        relpath = relpath.toLowerCase

        if (relpaths.contains(relpath)) {
          success = false
          Console.err.println(s"Overlapping classfiles: ${relpaths(relpath)} and $abspath")
        } else {
          relpaths(relpath) = abspath
        }
      })
    })

    if (!success) fail("Detected overlapping classfiles")
  }
}
