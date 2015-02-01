package scala.meta

import org.scalameta.adt._
import scala.meta.quasiquotes.Flavor

package quasiquotes {
  @root trait Flavor
}

package syntactic {
  package object quasiquotes {
    @leaf implicit object Enable extends Flavor
  }
}

package semantic {
  package object quasiquotes {
    @leaf implicit object Enable extends Flavor
  }
}
