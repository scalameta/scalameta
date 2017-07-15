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

  /** Returns the file contents as Buffer using blocking apis.
    *
    * NOTE: The actual return value is a Node.js buffer and not js.Array[Int].
    * However, both support .length and angle bracket access (foo[1]).
    **/
  def readFileSync(path: String): js.Array[Int] = js.native

  /** Returns the file contents as String using blocking apis */
  def readFileSync(path: String, encoding: String): String = js.native

  /** Writes file contents using blocking apis */
  def writeFileSync(path: String, contents: String): Unit = js.native

  /** Returns an array of filenames excluding '.' and '..'. */
  def readdirSync(path: String): js.Array[String] = js.native

  /** Returns an fs.Stats for path. */
  def lstatSync(path: String): JSStats = js.native

  /** Returns true if the file exists, false otherwise. */
  def existsSync(path: String): Boolean = js.native
}

/** Facade for nodejs class fs.Stats.
  *
  * @see https://nodejs.org/api/fs.html#fs_class_fs_stats
  */
@js.native
@JSImport("fs", Namespace)
class JSStats extends js.Any {
  def isFile(): Boolean = js.native
  def isDirectory(): Boolean = js.native
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
  def basename(path: String): String = js.native
  def dirname(path: String): String = js.native
  def relative(from: String, to: String): String = js.native
  def join(first: String, more: String*): String = js.native
}

object JSIO {
  def sep: String =
    if (isNode) JSPath.sep
    else "/"
  def delimiter: String =
    if (isNode) JSPath.delimiter
    else ":"
  def normalize(path: String): String =
    if (isNode) JSPath.normalize(path)
    else path
  def isAbsolute(path: String): Boolean =
    if (isNode) JSPath.isAbsolute(path)
    else path.startsWith("/")
  def resolve(path1: String, path2: String): String =
    if (isNode) JSPath.resolve(path1, path2)
    else s"$path1$sep$path2"

  private[io] def isNode = {
    JSFs != null
  }
  def inNode[T](f: => T): T =
    if (JSIO.isNode) f
    else {
      throw new IllegalStateException("This operation is not supported in this environment.")
    }
}
