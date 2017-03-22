package scala.meta

import scala.meta.contrib.implicits.{Converters, Equality, TreeExtensions, CommentExtensions}

package object contrib
  extends TreeExtensions
  with CommentExtensions
  with Equality
  with Converters
