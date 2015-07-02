package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.runtime.universe.{Type => Pt}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.parsers.Helpers.{XtensionTermOps => _, _}

// This module exposes a method that can convert scala.reflect trees into equivalent scala.meta trees.
//
// Unlike in the previous implementation, we don't care about preserving syntactic details of the original code:
// we just produce scala.meta trees for everything that we see (desugared forms or non-desugared forms alike),
// so that the result of the conversion captures all the semantics of the original code.
//
// In order to obtain a scala.meta tree that combines syntactic and semantic precision,
// you will need to use a dedicated module called `mergeTrees` that is capable of merging
// syntactically precise trees (obtained from parsing) and semantically precise treed (obtain from converting).
trait ToMtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  import g.Quasiquote

  def toMtree(in: g.Tree): m.Tree = {
    ???
  }
}