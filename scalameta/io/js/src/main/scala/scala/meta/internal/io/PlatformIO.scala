package scala.meta.internal.io

import scala.meta.io._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Namespace

/** Facade for npm package "shelljs".
  *
  * @see https://www.npmjs.com/package/shelljs
  */
@js.native
@JSImport("shelljs", Namespace)
object JSShell extends js.Any {

  /** Returns the current directory. */
  def pwd(): js.Object = js.native
}

/** Facade for native nodejs module "fs".
  *
  * @see https://nodejs.org/api/fs.html
  */
@js.native
@JSImport("fs", Namespace)
object JSFs extends js.Any {

  /** Returns the file contents using blocking apis */
  def readFileSync(path: String, encoding: String): js.Any = js.native

  /** Writes file contents using blocking apis */
  def writeFileSync(path: String, contents: String): Unit = js.native
}

/** Facade for native nodejs module "path".
  *
  * @see https://nodejs.org/api/path.html
  */
@js.native
@JSImport("path", Namespace)
object JSPath extends js.Any {
  def sep: String = js.native
  def delimiter: String = js.native
  def isAbsolute(path: String): Boolean = js.native
  def resolve(paths: String*): String = js.native
  def normalize(path: String): String = js.native
}

object PlatformIO {
  private[io] def isNode = JSFs != null
}