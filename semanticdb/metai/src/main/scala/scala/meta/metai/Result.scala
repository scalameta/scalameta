package scala.meta.metai

import scala.collection.immutable.ListMap
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

final class Result private (val status: ListMap[AbsolutePath, Boolean]) {
  def isSuccess: Boolean = {
    status.forall(_._2)
  }

  def classpath: Option[Classpath] = {
    if (isSuccess) Some(Classpath(status.toList.map(_._1)))
    else None
  }

  override def toString: String = {
    s"Result($status)"
  }
}

object Result {
  def apply(status: ListMap[AbsolutePath, Boolean]): Result = {
    new Result(status)
  }
}
