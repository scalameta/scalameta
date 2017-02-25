package scala.meta

import scala.meta.contrib.implicits.{Converters, Equality, TreeExtensions}

package object contrib
  extends TreeExtensions
  with Equality
  with Converters
