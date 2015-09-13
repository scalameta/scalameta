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

trait ToMlit extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionGconstToMlit(gconst: g.Constant) {
    def toMlit: m.Lit = {
      val msytree = gconst.value match {
        case null => m.Lit.Null()
        case () => m.Lit.Unit()
        case v: Boolean => m.Lit.Bool(v)
        case v: Byte => m.Lit.Byte(v)
        case v: Short => m.Lit.Short(v)
        case v: Int => m.Lit.Int(v)
        case v: Long => m.Lit.Long(v)
        case v: Float => m.Lit.Float(v)
        case v: Double => m.Lit.Double(v)
        case v: String => m.Lit.String(v)
        case v: Char => m.Lit.Char(v)
        case v: g.Type => unreachable(debug(gconst))
        case v: g.Symbol => unreachable(debug(gconst))
      }
      msytree.withMattrs(gconst.tpe).forceTypechecked
    }
  }
}