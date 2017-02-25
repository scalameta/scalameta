package scala.meta

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.meta.contrib.conversion.Converters

package object contrib
  extends TreeExtensions
  with Converters
