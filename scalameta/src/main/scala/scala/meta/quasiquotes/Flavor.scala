package scala.meta

import org.scalameta.adt._
import scala.meta.quasiquotes.Flavor
import scala.meta.internal.quasiquotes.EnableMacros
import scala.language.experimental.{macros => prettyPlease}
import scala.reflect.macros.blackbox.Context

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
    @leaf object EnableImpl extends Flavor
    implicit def Enable: EnableImpl.type = macro EnableMacros.impl
  }
}

package internal.quasiquotes {
  class EnableMacros(val c: Context) {
    def impl: c.Tree = {
      import c.universe._
      val TermQuote = "shadow scala.meta quasiquotes"
      val context = c.inferImplicitValue(typeOf[scala.meta.semantic.Context], silent = true)
      if (context.isEmpty) c.abort(c.enclosingPosition, "semantic quasiquotes require an implicit scala.meta.semantic.Context")
      q"_root_.scala.meta.semantic.quasiquotes.`package`.EnableImpl"
    }
  }
}
