package scala.meta.internal.io

import java.nio.file.Path

import scala.util.Try

object ProcessIO {

  def exit(code: Int): Nothing = PlatformProcessIO.exit(code)

  /** Runs the command and returns its output lines, or a failure if the command exits non-zero. */
  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[Seq[String]] = PlatformProcessIO
    .runArgv(cmd, cwd)

}
