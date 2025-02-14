package scala.meta.internal.io

object PlatformPathIO {
  def workingDirectoryString: String = JSIO.cwd()
}
