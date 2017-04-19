package scala.meta
package semantic
package v1

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.semantic.v1.mirrors.OfflineMirror

trait Mirror {
  def dialect: Dialect
  def sources: Seq[Source]
  def database: Database
  def symbol(ref: Ref): Completed[Symbol]
  def denot(sym: Symbol): Completed[Denotation]
}

object Mirror {
  /** Construct a Mirror from a persisted semantic database.
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
    new OfflineMirror(classpath, sourcepath)

  /** Automatically construct of a Mirror in a properly setup build integration.
    *
    * Recommended constructor if using a build integration like sbt-scalahost.
    *
    * @throws RuntimeException if system properties is missing necessary values, for
    *                          example when the build integration is improperly setup.
    */
  def apply(): Mirror =
    apply(sys.props("scalameta.classpath"), sys.props("scalameta.sourcepath"))
}