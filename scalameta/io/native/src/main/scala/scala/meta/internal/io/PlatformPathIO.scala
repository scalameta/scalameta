package scala.meta.internal.io

object PlatformPathIO {
  def workingDirectoryString: String = sys.props("user.dir")
}
