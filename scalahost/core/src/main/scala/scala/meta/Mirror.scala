package scala.meta

import scala.collection.immutable.Seq
import scala.meta.internal.scalahost.v1.offline
import scala.meta.internal.scalahost.v1.online
import scala.tools.nsc.Global

object Mirror {

  /** Construct online Mirror from scalac Global instance.
    *
    * You may wish to use this constructor from within, for example:
    * - a compiler plugin
    * - the presentation compiler
    */
  def apply(global: Global): Mirror = new online.Mirror(global)

  /** Construct offline Mirror from a persisted semantic database.
    *
    * @param classpath java.io.File.pathSeparator separated list of jar files
    *                 or directories containing classfiles and `semanticdb` files.
    *                 The `semanticdb` files are emitted by the scalahost-nsc
    *                 compiler plugin and are necessary for the semantic API to
    *                 function. The classfiles + jar files are necessary for
    *                 runtime compilation of quasiquotes when extracting
    *                 symbols (that is, `q"scala.Predef".symbol`).
    *
    *                 Example:
    *                    "foo/target/classes:foo.jar:/home/.ivy2/cache/bar.jar".
    * @param sourcepath java.io.File.pathSeparator separated list of
    *                  Scala source files OR directories containing Scala
    *                  source files. For example
    *
    *                  Example:
    *                      "src/main/scala:build/Bar.scala".
    */
  def apply(classpath: String, sourcepath: String): Mirror =
    new offline.Mirror(classpath, sourcepath)

  /** Automatic construction of an offline Mirror in a properly setup build integration.
    *
    * Recommended constructor if using a build integration like sbt-scalahost.
    *
    * @throws RuntimeException if system properties is missing necessary values, for
    *                          example when the build integration is improperly setup.
    */
  def apply(): Mirror =
    apply(sys.props("scalameta.classpath"), sys.props("scalameta.sourcepath"))
}
