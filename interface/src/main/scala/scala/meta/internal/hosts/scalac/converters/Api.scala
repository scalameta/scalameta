package scala.meta
package internal.hosts.scalac
package converters

abstract class Api(global: scala.tools.nsc.Global)
extends ToM
   with ToMannot
   with ToMmember
   with ToMtree
   with ToMtype
   with ToGtree
   with ToGtype
   with Attributes
   with SymbolTables
   with TrickyConversions
   with Caches