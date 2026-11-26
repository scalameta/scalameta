package scala.meta.internal.io

import java.nio.file.Path

import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}

object PlatformProcessIO {

  def exit(code: Int): Nothing = sys.exit(code)

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[Seq[String]] = {
    val out = Seq.newBuilder[String]
    val err = new StringBuilder()
    val logger = ProcessLogger(out += _, err.append("\n> ").append(_))
    def failed(e: Throwable) = {
      val msg = cmd.addString(new StringBuilder(), "Failed to run '", " ", "'. Error: ").append(err)
        .append('\n')
      Failure(new IllegalStateException(msg.toString(), e))
    }
    try {
      val exit = Process(cmd, cwd.map(_.toFile)).!(logger)
      if (exit != 0) failed(new RuntimeException("exit code " + exit)) else Success(out.result())
    } catch { case e: Throwable => failed(e) }
  }

}
