package scala.meta
package internal.hosts.scalac
package converters

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}

abstract class Api(global: ScalaGlobal)
extends ToM
   with ToMannot
   with ToMmember
   with ToMtree
   with ToMtype
   with ToGtree
   with ToGtype
   with Attributes
   with SymbolTables
   with Caches {
  implicit val c: ScalametaSemanticContext
}