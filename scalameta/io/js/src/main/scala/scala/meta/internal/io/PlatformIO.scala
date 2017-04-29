package scala.meta.internal.io

import scala.meta.io._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Namespace

import java.nio.charset.Charset

/** Facade for npm package "shelljs".
  *
  * @see https://www.npmjs.com/package/shelljs
  */
@js.native
@JSImport("shelljs", Namespace)
object Shell extends js.Any {

  /** Returns the current directory. */
  def pwd(): js.Object = js.native
}

/** Facade for native nodejs module "fs".
  *
  * @see https://nodejs.org/api/fs.html
  */
@js.native
@JSImport("fs", Namespace)
object Fs extends js.Any {

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
object Path extends js.Any {
  def sep: String = js.native
  def delimiter: String = js.native
  def isAbsolute(path: String): Boolean = js.native
}

object PlatformIO {
  private def isNode = Fs != null
  def workingDirectory: AbsolutePath =
    if (isNode) AbsolutePath(Shell.pwd().toString).get
    else AbsolutePath(fileSeparator).get
  def slurp(path: AbsolutePath, charset: Charset): String =
    if (isNode) Fs.readFileSync(path.absolute, charset.toString).toString
    else {
      throw new IllegalStateException(
        "Slurping from an AbsolutePath is not supported in this environment.")
    }
  def slurp(path: AbsolutePath): String = slurp(path, Charset.forName("UTF8"))
  def fileSeparator: String = Path.sep
  def pathSeparator: String = Path.delimiter
  def isAbsolutePath(path: String): Boolean = Path.isAbsolute(path)
}
