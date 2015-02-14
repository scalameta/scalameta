package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.collections._
import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.internal.Flags._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.internal.{ast => p}
import scala.meta.internal.{hygiene => h}
import scala.meta.ui.{Exception => SemanticException}
import java.util.UUID.randomUUID

// This module tracks the correspondence between scala.reflect and scala.meta symbols.
// It can also do automatic conversion between globally visible symbols,
// but it needs help when dealing with local symbols.
trait SymbolTables extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected class SymbolTable {
    private val symCache = TwoWayCache[l.Symbol, h.Symbol]()

    // TODO: `convert` is somewhat copy/pasted from core/quasiquotes/Macros.scala
    // however, there's no way for us to share those implementations until we bootstrap
    def convert(lsym: l.Symbol): h.Symbol = symCache.getOrElseUpdate(lsym, {
      def isGlobal(gsym: g.Symbol): Boolean = {
        def definitelyLocal = gsym == g.NoSymbol || gsym.name.toString.startsWith("<local ") || (gsym.owner.isMethod && !gsym.isParameter)
        def isParentGlobal = gsym.hasPackageFlag || isGlobal(gsym.owner)
        !definitelyLocal && isParentGlobal
      }
      def signature(gsym: g.Symbol): h.Signature = {
        if (gsym.isMethod && !gsym.asMethod.isGetter) h.Signature.Method(gsym.jvmsig)
        else if (gsym.isTerm || (gsym.hasFlag(DEFERRED | EXISTENTIAL) && gsym.name.endsWith(g.nme.SINGLETON_SUFFIX))) h.Signature.Term
        else if (gsym.isType) h.Signature.Type
        else unreachable
      }
      if (sys.props("convert.debug") != null) println(lsym)
      val gsym = lsym.gsymbol
      require(!gsym.isModuleClass && !gsym.isPackageClass)
      if (gsym == g.NoSymbol) h.Symbol.Zero
      else if (gsym == g.rootMirror.RootPackage) h.Symbol.Root
      else if (gsym == g.rootMirror.EmptyPackage) h.Symbol.Empty
      else if (isGlobal(gsym)) h.Symbol.Global(convert(gsym.owner.toLogical), gsym.name.decodedName.toString, signature(gsym))
      else h.Symbol.Local(randomUUID().toString)
    })

    def lookupOrElseUpdate(lsym: l.Symbol, hsym: => h.Symbol): h.Symbol = symCache.getOrElseUpdate(lsym, hsym)

    def convert(hsym: h.Symbol): l.Symbol = symCache.getOrElseUpdate(hsym, {
      if (sys.props("convert.debug") != null) println(hsym)
      def resolve(lsym: l.Symbol, name: String, hsig: h.Signature): l.Symbol = {
        val gsym = hsig match {
          case h.Signature.Type => lsym.gsymbol.info.decl(g.TypeName(name)).asType
          case h.Signature.Term => lsym.gsymbol.info.decl(g.TermName(name)).suchThat(galt => galt.isGetter || !galt.isMethod).asTerm
          case h.Signature.Method(jvmsig) => lsym.gsymbol.info.decl(g.TermName(name)).suchThat(galt => galt.isMethod && galt.jvmsig == jvmsig).asTerm
          case h.Signature.TypeParameter => lsym.gsymbol.typeParams.filter(_.name.toString == name).head
          case h.Signature.TermParameter => lsym.gsymbol.paramss.flatten.filter(_.name.toString == name).head
        }
        gsym.toLogical
      }
      hsym match {
        case h.Symbol.Zero => l.None
        case h.Symbol.Root => l.Package(g.rootMirror.RootPackage, g.rootMirror.RootClass)
        case h.Symbol.Empty => l.Package(g.rootMirror.EmptyPackage, g.rootMirror.EmptyPackageClass)
        case h.Symbol.Global(howner, name, hsig) => resolve(convert(howner), name, hsig)
        case h.Symbol.Local(id) => throw new SemanticException(s"implementation restriction: internal cache has no symbol associated with $hsym")
      }
    })

    def lookupOrElseUpdate(hsym: h.Symbol, lsym: => l.Symbol): l.Symbol = symCache.getOrElseUpdate(hsym, lsym)
  }
}