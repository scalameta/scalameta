package scala.meta.metacp

import scala.collection.immutable.ListMap
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

final class Result private (
    val status: ListMap[AbsolutePath, Option[AbsolutePath]],
    val scalaLibrarySynthetics: Option[AbsolutePath]) {
  def isSuccess: Boolean = {
    status.forall(_._2.nonEmpty)
  }

  def classpath: Option[Classpath] = {
    if (isSuccess) Some(Classpath(status.toList.flatMap(_._2) ++ scalaLibrarySynthetics))
    else None
  }

  override def toString: String = {
    s"Result($status, $scalaLibrarySynthetics)"
  }
}

object Result {
  def apply(
      status: ListMap[AbsolutePath, Option[AbsolutePath]],
      scalaLibrarySynthetics: Option[AbsolutePath]): Result = {
    new Result(status, scalaLibrarySynthetics)
  }
}
