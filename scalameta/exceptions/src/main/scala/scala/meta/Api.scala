package scala.meta

private[meta] trait ExceptionApi {
  // TODO: Any => Position
  def abort(msg: String): Nothing = throw new AbortException(msg)
  def abort(pos: Any, msg: String): Nothing = throw new AbortException(pos, msg)
}