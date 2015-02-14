package scala.meta
package internal.hosts.scalac
package converters

abstract class Api(global: scala.tools.nsc.Global)
extends ToP
   with ToPannot
   with ToPmember
   with ToPtree
   with ToPtype
   with ToGtree
   with ToGtype
   with Attributes
   with SymbolTables
   with TrickyConversions
   with Caches