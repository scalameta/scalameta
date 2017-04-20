package scala.meta

import scala.meta.contrib.implicits._
import scala.meta.contrib.instances._

package object contrib
  extends TreeExtensions
  with SetExtensions
  with CommentExtensions
  with Equality
  with Converters
  with ExtractStatInstances
  with ExtractStatSubtypeInstances
  with ExtractModsInstances
  with ExtractAnnotationInstances
  with ExtractExtensions

