package scala.meta.internal.io

import scala.scalajs.js.Dynamic.global

object PlatformPathIO {
  def workingDirectoryString: String = JSIO.cwd()

  def urlEncode(str: String): String = global.encodeURIComponent(str).asInstanceOf[String]
  def urlDecode(str: String): String = global.decodeURIComponent(str).asInstanceOf[String]
}
