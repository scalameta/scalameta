package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.hosts.scalac.reflect._

trait ToMlit extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionGconstToMlit(gconst: g.Constant) {
    def toMlit: m.Lit = {
      require(!gconst.value.isInstanceOf[g.Type] && !gconst.value.isInstanceOf[g.Symbol])
      val msytree = m.Lit(gconst.value)
      msytree.withMattrs(gconst.tpe).forceTypechecked
    }
  }
}