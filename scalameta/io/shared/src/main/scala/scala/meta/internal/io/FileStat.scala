package scala.meta.internal.io

trait FileStat {

  def isDirectory: Boolean
  def isRegularFile: Boolean
  def isSymlink: Boolean

}
