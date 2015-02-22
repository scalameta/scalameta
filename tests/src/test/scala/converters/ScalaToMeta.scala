import org.scalatest._
import java.net._
import java.io._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.hosts.scalac.Scalahost
import org.scalameta.reflection._

class ScalaToMeta extends FunSuite {
  def typecheckConvertAndPrettyprint(code: String, debug: Boolean): String = {
    val pluginJar = System.getProperty("sbt.paths.plugin.jar")
    val compilationClasspath = System.getProperty("sbt.paths.tests.classpath").split(File.pathSeparatorChar.toString).map(path => new URL("file://" + path))
    val options = "-cp " + System.getProperty("sbt.paths.tests.classpath") + " -Xplugin:" + pluginJar + " -Xplugin-require:scalahost"
    implicit val c = Scalahost.mkStandaloneContext(options)
    val m = c.define(code)
    if (debug) println(m.show[Code])
    if (debug) println(m.show[Raw])
    m.show[Code]
  }

  def runScalaToMetaTest(dirPath: String): Unit = {
    def resource(label: String) = dirPath + File.separatorChar + label + ".scala"
    def slurp(label: String) = scala.io.Source.fromFile(new File(resource(label))).mkString.trim
    def dump(label: String, content: String) = {
      val w = new BufferedWriter(new FileWriter(resource(label)))
      w.write(content)
      w.close()
    }
    def exists(label: String) = new File(resource(label)).exists
    def delete(label: String) = {
      val f = new File(resource(label))
      if (f.exists) f.delete
    }
    delete("Actual")
    val actualResult = typecheckConvertAndPrettyprint(slurp("Original"), debug = false)
    if (!exists("Expected")) dump("Expected", "")
    val expectedResult = slurp("Expected")
    // TODO: would also be nice to test structural equivalence of reflect -> meta converted trees and meta parsed trees
    // unfortunately much of the trivia is lost during reflect parsing, so we can only dream of structural equivalence...
    if (actualResult != expectedResult) {
      dump("Actual", actualResult)
      fail(s"see ${resource("Actual")} for details")
    }
  }

  val resourceDir = new File(System.getProperty("sbt.paths.tests.resources") + File.separatorChar + "ScalaToMeta")
  val testDirs = resourceDir.listFiles().filter(_.listFiles().nonEmpty).filter(!_.getName().endsWith("_disabled"))
  testDirs.foreach(testDir => test(testDir.getName)(runScalaToMetaTest(testDir.getAbsolutePath)))
}
