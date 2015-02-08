package scala.meta

import org.scalameta.adt._
import scala.meta.quasiquotes.Flavor
import scala.annotation.implicitNotFound

package quasiquotes {
  @implicitNotFound("choose the flavor of quasiquotes by importing either scala.meta.syntactic.quasiquotes._ or scala.meta.semantic.quasiquotes._")
  @root trait Flavor
}

package syntactic {
  package object quasiquotes {
    @leaf implicit object enableSyntacticQuasiquotes extends Flavor
  }
}

package semantic {
  package object quasiquotes {
    @leaf implicit object enableSemanticQuasiquotes extends Flavor
  }
}
