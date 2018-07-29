package scala.meta.metai

import scala.meta.io.AbsolutePath

final class Result private (val status: Map[AbsolutePath, Boolean]) {
  def success: Boolean = status.forall(_._2)
}

object Result {
  def apply(status: Map[AbsolutePath, Boolean]): Result = {
    new Result(status)
  }
}
