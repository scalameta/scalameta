package scala.meta

import scala.meta.contrib.implicits._
import scala.meta.contrib.instances._

package object contrib
  extends TreeExtensions
  with CommentExtensions
  with Equality
  with Converters
  with ExtractStatInstances
  with ExtractStatSubtypeInstances
  with ExtractPimps

