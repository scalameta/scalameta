package scala.meta.tests.io

import scala.meta.internal.io.{JSFs, JSOS, JSPath}
import scala.meta.io.AbsolutePath

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._
import scala.util.Try

/** Builds and removes a tree of files in a temporary directory. */
object TestTree {

  @js.native
  @JSImport("fs", "chmodSync")
  private object ChmodSync extends js.Object {
    def apply(path: String, mode: Int): Unit = js.native
  }

  /** Whether the owner can still list a directory with mode `000`, as root can. */
  lazy val ignoresPermissions: Boolean = {
    val dir = mkdtemp()
    chmod(dir, "---------")
    try Try(readdir(dir)).isSuccess
    finally {
      chmod(dir, "rwx------")
      delete(dir)
    }
  }

  def mkdtemp(): AbsolutePath =
    AbsolutePath(JSFs.mkdtempSync(JSPath.join(JSOS.tmpdir(), "scalameta-")))

  def mkdir(path: AbsolutePath): Unit = JSFs.mkdirSync(path.toString)

  def writeBytes(path: AbsolutePath, bytes: Array[Byte]): Unit = JSFs
    .writeFileSync(path.toString, new Uint8Array(bytes.toTypedArray.buffer))

  def symlink(link: AbsolutePath, target: AbsolutePath): Unit = JSFs
    .symlinkSync(target.toString, link.toString)

  /** @param permissions such as `rwxr-x---` */
  def chmod(path: AbsolutePath, permissions: String): Unit =
    ChmodSync(path.toString, Integer.parseInt(permissions.map(c => if (c == '-') '0' else '1'), 2))

  /**
   * Returns the names of the entries of a directory, in the order that the file system lists them.
   */
  def readdir(path: AbsolutePath): List[String] = JSFs.readdirSync(path.toString).toList

  def delete(root: AbsolutePath): Unit = JSFs
    .rmSync(root.toString, js.Dynamic.literal(recursive = true, force = true))

}
