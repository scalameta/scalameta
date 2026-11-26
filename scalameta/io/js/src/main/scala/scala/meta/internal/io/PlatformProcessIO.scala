package scala.meta.internal.io

import java.nio.file.Path

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.util.{Failure, Success, Try}

object PlatformProcessIO {

  def exit(code: Int): Nothing = {
    JSIO.process.exit(code)
    throw new Throwable() // just like JVM's sys.exit
  }

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[Seq[String]] = {
    val options = cwd
      .fold(js.Dictionary[js.Any]())(cwd => js.Dictionary[js.Any]("cwd" -> cwd.toString))
    val result = JSChildProcess.spawnSync(cmd.head, cmd.tail.toJSArray, options)
    if (result.status.asInstanceOf[Int] == 0)
      Success(result.stdout.toString.trim.linesIterator.toSeq)
    else {
      val msg = s"Failed to run '${cmd.mkString(" ")}'. Error:\n${result.stderr}\n"
      Failure(js.JavaScriptException(msg))
    }
  }

}
