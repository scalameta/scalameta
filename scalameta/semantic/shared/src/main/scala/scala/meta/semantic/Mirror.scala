package scala.meta
package semantic

import scala.meta.io._

trait Mirror {
  def database: Database
}

object Mirror {
  def apply(classpath: Classpath, sourcepath: Sourcepath): Mirror = {
    Database.load(classpath, sourcepath)
  }

  def apply(classpath: Classpath): Mirror = {
    Database.load(classpath)
  }
}