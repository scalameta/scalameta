package scala.meta

import scala.meta.contrib.implicits.{CommentExtensions, Converters, EqualityImplicits, TreeExtensions}
import scala.meta.contrib.instances.EqualityInstances

package object contrib
  extends TreeExtensions
  with CommentExtensions
  with EqualityImplicits
  with EqualityInstances
  with Converters
