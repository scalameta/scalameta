package scala.meta

import scala.meta.contrib.implicits.implicits
import scala.meta.contrib.instances.instances

package object contrib
  extends implicits
  with instances
  with TreeExtractors

