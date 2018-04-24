package scala.meta.internal.io

import scala.meta.io._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Namespace

/** Facade for the native nodejs process API
  *
  * The process object is a global that provides information about, and
  * control over, the current Node.js process. As a global, it is always
  * available to Node.js applications without using require().
  *
  * @see https://nodejs.org/api/process.html
  */
@js.native
trait JSProcess extends js.Any {
  def cwd(): String = js.native
}

/** Facade for the native nodejs modules API
  *
  * @see https://nodejs.org/api/modules.html
  */
@js.native
trait JSModules extends js.Any {
  /** Load a module by ID or path.
    *
    * @see https://nodejs.org/api/modules.html#modules_require
    */
  def require[T](path: String): T = js.native
}

/** Facade for native nodejs module "fs".
  *
  * @see https://nodejs.org/api/fs.html
  */
@js.native
trait JSFs extends js.Any {

  /** Returns the file contents as Buffer using blocking apis.
    *
    * NOTE: The actual return value is a Node.js buffer and not js.Array[Int].
    * However, both support .length and angle bracket access (foo[1]).
    **/
  def readFileSync(path: String): js.Array[Int] = js.native

  /** Returns the file contents as String using blocking apis */
  def readFileSync(path: String, encoding: String): String = js.native

  /** Writes file contents using blocking apis */
  def writeFileSync(path: String, buffer: js.Array[Int]): Unit = js.native

  /** Returns an array of filenames excluding '.' and '..'. */
  def readdirSync(path: String): js.Array[String] = js.native

  /** Returns an fs.Stats for path. */
  def lstatSync(path: String): JSStats = js.native

  /** Returns true if the file exists, false otherwise. */
  def existsSync(path: String): Boolean = js.native

  /** Synchronously creates a directory. */
  def mkdirSync(path: String): Unit = js.native
}

/** Facade for nodejs class fs.Stats.
  *
  * @see https://nodejs.org/api/fs.html#fs_class_fs_stats
  */
@js.native
trait JSStats extends js.Any {
  def isFile(): Boolean = js.native
  def isDirectory(): Boolean = js.native
}

/** Facade for native nodejs module "path".
  *
  * @see https://nodejs.org/api/path.html
  */
@js.native
trait JSPath extends js.Any {
  def sep: String = js.native
  def delimiter: String = js.native
  def isAbsolute(path: String): Boolean = js.native
  def parse(path: String): JSPath = js.native
  def resolve(paths: String*): String = js.native
  def normalize(path: String): String = js.native
  def basename(path: String): String = js.native
  def dirname(path: String): String = js.native
  def root: String = js.native
  def relative(from: String, to: String): String = js.native
  def join(first: String, more: String*): String = js.native
}

object JSIO {
  private[io] val process: JSProcess = js.Dynamic.global.process.asInstanceOf[JSProcess]
  def isNode = !js.isUndefined(process) && !js.isUndefined(process.cwd)
  private[io] lazy val module: JSModules = js.Dynamic.global.asInstanceOf[JSModules]
  lazy val fs: JSFs = module.require("fs")
  lazy val path: JSPath = module.require("path")

  def inNode[T](f: => T): T =
    if (JSIO.isNode) f
    else {
      throw new IllegalStateException("This operation is not supported in this environment.")
    }

  def cwd(): String =
    if (isNode) process.cwd()
    else "/"

  def exists(path: String): Boolean =
    if (isNode) fs.existsSync(path)
    else false

  def isFile(path: String): Boolean =
    exists(path) && fs.lstatSync(path).isFile()

  def isDirectory(path: String): Boolean =
    exists(path) && fs.lstatSync(path).isDirectory()
}
