package scala.meta.internal.io

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Namespace

/**
 * Facade for the native nodejs process API
 *
 * The process object is a global that provides information about, and control over, the current
 * Node.js process. As a global, it is always available to Node.js applications without using
 * require().
 *
 * @see
 *   https://nodejs.org/api/process.html
 */
@js.native
trait JSProcess extends js.Any {
  def cwd(): String = js.native
}

/**
 * Facade for native nodejs module "fs".
 *
 * @see
 *   https://nodejs.org/api/fs.html
 */
@js.native
@JSImport("fs", Namespace)
object JSFs extends js.Any {

  /**
   * Returns the file contents as Buffer using blocking apis.
   *
   * NOTE: The actual return value is a Node.js buffer and not js.Array[Int]. However, both support
   * .length and angle bracket access (foo[1]).
   */
  def readFileSync(path: String): js.Array[Int] = js.native

  /** Returns the file contents as String using blocking apis */
  def readFileSync(path: String, encoding: String): String = js.native

  /** Reads file asynchronously, invokes callback when done */
  def readFile(
      path: String,
      encoding: String,
      callback: js.Function2[js.Error, String, Unit]
  ): Unit = js.native

  /** Writes file contents using blocking apis */
  def writeFileSync(path: String, buffer: js.Array[Int]): Unit = js.native
  def writeFileSync(path: String, data: js.typedarray.Uint8Array): Unit = js.native
  def writeFileSync(path: String, data: String, encoding: js.UndefOr[String] = js.undefined): Unit =
    js.native

  /** Writes file asynchronously */
  def writeFile(
      path: String,
      data: String,
      encoding: String,
      callback: js.Function1[js.Error, Unit]
  ): Unit = js.native

  /** Returns an array of filenames excluding '.' and '..'. */
  def readdirSync(path: String): js.Array[String] = js.native

  /** Returns an fs.Stats for path. */
  def lstatSync(path: String): JSStats = js.native
  def statSync(path: String): JSStats = js.native

  /** Returns true if the file exists, false otherwise. */
  def existsSync(path: String): Boolean = js.native

  /** creates a symlink */
  def symlinkSync(
      path: String, // real path
      link: String, // link path
      /** type is file, dir, junction (windows) */
      `type`: js.UndefOr[String] = js.undefined
  ): Unit = js.native

  /** Synchronously creates a directory. */
  def mkdirSync(path: String): Unit = js.native
  def mkdirSync(path: String, options: js.UndefOr[js.Dynamic] = js.undefined): Unit = js.native
  def mkdtempSync(prefix: String): String = js.native

  /** e.g: rmSync(path, js.Dynamic.literal(recursive = true, force = true)) */
  def rmSync(path: String, options: js.UndefOr[js.Dynamic] = js.undefined): Unit = js.native
  def unlinkSync(path: String): Unit = js.native

  /** moves files */
  def renameSync(oldPath: String, newPath: String): Unit = js.native
}

@js.native
@JSImport("os", JSImport.Namespace)
object JSOS extends js.Object {
  def tmpdir(): String = js.native
  def homedir(): String = js.native
}

/**
 * Facade for nodejs class fs.Stats.
 *
 * @see
 *   https://nodejs.org/api/fs.html#fs_class_fs_stats
 */
@js.native
@JSImport("fs", Namespace)
class JSStats extends js.Any {
  def isFile(): Boolean = js.native
  def isDirectory(): Boolean = js.native
  def isSymbolicLink(): Boolean = js.native
  val atime: js.Date = js.native
  val ctime: js.Date = js.native
  val mtime: js.Date = js.native
}

/**
 * Facade for native nodejs module "path".
 *
 * @see
 *   https://nodejs.org/api/path.html
 */
@js.native
@JSImport("path", Namespace)
object JSPath extends js.Any {
  def sep: String = js.native
  def delimiter: String = js.native
  def isAbsolute(path: String): Boolean = js.native
  def parse(path: String): JSPath.type = js.native
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
  def isNode = !js.isUndefined(process) && !js.isUndefined(process.cwd())

  def inNode[T](f: => T): T =
    if (JSIO.isNode) f
    else throw new IllegalStateException("This operation is not supported in this environment.")

  def cwd(): String = if (isNode) process.cwd() else "/"

  def exists(path: String): Boolean = if (isNode) JSFs.existsSync(path) else false

  def isFile(path: String): Boolean = exists(path) && JSFs.lstatSync(path).isFile()

  def isDirectory(path: String): Boolean = exists(path) && JSFs.lstatSync(path).isDirectory()

  def readStdinAsync: Future[String] = {
    val inputStream = js.Dynamic.global.process.stdin
    val promise = Promise[String]()
    val readBuffer = new StringBuilder

    inputStream.on("data", (data: String) => readBuffer.append(data))

    inputStream.on("end", () => promise.trySuccess(readBuffer.toString()))

    inputStream.on(
      "error",
      (err: js.Any) =>
        promise.tryFailure(new RuntimeException(err match {
          case e: js.Error => e.message
          case _ => err.toString
        }))
    )

    promise.future
  }

}
