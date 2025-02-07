package scala.meta.internal.io

object PlatformPathIO {
  def workingDirectoryString: String = sys.props("user.dir")

  def urlEncode(str: String): String = java.net.URLEncoder.encode(str, "UTF-8")
  def urlDecode(str: String): String = java.net.URLDecoder.decode(str, "UTF-8")
}
