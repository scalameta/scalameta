package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}

trait ToG extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionNamePrefix(mname: m.Name) {
    def toGprefix = mname.denot.prefix match {
      case s.Prefix.Zero => g.NoPrefix
      case s.Prefix.Type(mtpe) => mtpe.require[m.Type].toGtype
    }
  }

  protected implicit class XtensionMemberPrefix(mmember: m.Member) {
    def toGprefix = mmember.name.require[m.Name].toGprefix
  }
}