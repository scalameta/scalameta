package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.flags._

// This a random grab bag of utilities.
// Ideally, we'd find better places for them than this file.
trait ToM extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected def mfakector(gtpe: g.Type): m.Ctor.Primary = {
    val ctorTpe = g.MethodType(Nil, gtpe)
    val ctorOwner = symbolTable.convert(gtpe.typeSymbol.toLogical)
    val ctorSym = s.Symbol.Global(ctorOwner, "<init>", s.Signature.Method(ctorTpe.jvmsig))
    val ctorDenot = s.Denotation.Single(s.Prefix.Type(gtpe.toMtype), ctorSym)
    val ctorName = m.Ctor.Name(gtpe.typeSymbol.displayName).withMattrs(ctorDenot, ctorTpe)
    m.Ctor.Primary(Nil, ctorName, List(List()))
  }
}