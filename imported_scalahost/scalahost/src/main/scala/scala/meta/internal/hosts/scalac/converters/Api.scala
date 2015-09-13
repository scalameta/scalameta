package scala.meta
package internal.hosts.scalac
package converters

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}

abstract class Api(global: ScalaGlobal)
extends ToM
   with ToMannot
   with ToMattrs
   with ToMlit
   with ToMmember
   with ToMname
   with ToMtree
   with ToMtype
   with ToGprefix
   with ToGsymbol
   with ToGtree
   with ToGtype
   with SymbolTables
   with Caches {
  implicit val c: ScalametaSemanticContext
}