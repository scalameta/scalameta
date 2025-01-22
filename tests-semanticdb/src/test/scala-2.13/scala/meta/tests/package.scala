package scala.meta

import munit.diff.DiffOptions

package object tests {
  implicit val diffOptions: DiffOptions = DiffOptions.withShowLines(true).withContextSize(10)
    .withObtainedAsStripMargin(false)
}
