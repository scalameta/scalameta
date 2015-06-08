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
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import java.util.UUID.randomUUID

// This module tracks the correspondence between scala.reflect and scala.meta symbols.
// It can also do automatic conversion between globally visible symbols,
// but it needs help when dealing with local symbols.
trait SymbolTables extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected class SymbolTable {
    private val symCache = TwoWayCache[l.Symbol, s.Symbol]()

    // TODO: `convert` is somewhat copy/pasted from core/quasiquotes/Macros.scala
    // however, there's no way for us to share those implementations until we bootstrap
    def convert(lsym: l.Symbol): s.Symbol = symCache.getOrElseUpdate(lsym, {
      def isGlobal(gsym: g.Symbol): Boolean = {
        def definitelyLocal = gsym == g.NoSymbol || gsym.name.toString.startsWith("<local ") || (gsym.owner.isMethod && !gsym.isParameter)
        def isParentGlobal = gsym.hasPackageFlag || isGlobal(gsym.owner)
        !definitelyLocal && isParentGlobal
      }
      def signature(gsym: g.Symbol): s.Signature = {
        if (gsym.isMethod && !gsym.asMethod.isGetter) s.Signature.Method(gsym.jvmsig)
        else if (gsym.isTerm || (gsym.hasFlag(DEFERRED | EXISTENTIAL) && gsym.name.endsWith(g.nme.SINGLETON_SUFFIX))) s.Signature.Term
        else if (gsym.isType) s.Signature.Type
        else unreachable(debug(gsym, gsym.flags, gsym.getClass, gsym.owner))
      }
      if (sys.props("convert.debug") != null) println(lsym)
      val gsym = lsym.gsymbol
      require(!gsym.isModuleClass && !gsym.isPackageClass)
      if (gsym == g.NoSymbol) s.Symbol.Zero
      else if (gsym == g.rootMirror.RootPackage) s.Symbol.Root
      else if (gsym == g.rootMirror.EmptyPackage) s.Symbol.Empty
      else if (isGlobal(gsym)) s.Symbol.Global(convert(gsym.owner.toLogical), gsym.name.decodedName.toString, signature(gsym))
      else s.Symbol.Local(randomUUID().toString)
    })

    def lookupOrElseUpdate(lsym: l.Symbol, ssym: => s.Symbol): s.Symbol = symCache.getOrElseUpdate(lsym, ssym)

    def convert(ssym: s.Symbol): l.Symbol = symCache.getOrElseUpdate(ssym, {
      if (sys.props("convert.debug") != null) println(ssym)
      def resolve(lsym: l.Symbol, name: String, hsig: s.Signature): l.Symbol = {
        val gsym = hsig match {
          case s.Signature.Type => lsym.gsymbol.info.decl(g.TypeName(name)).asType
          case s.Signature.Term => lsym.gsymbol.info.decl(g.TermName(name)).suchThat(galt => galt.isGetter || !galt.isMethod).asTerm
          case s.Signature.Method(jvmsig) => lsym.gsymbol.info.decl(g.TermName(name)).suchThat(galt => galt.isMethod && galt.jvmsig == jvmsig).asTerm
          case s.Signature.TypeParameter => lsym.gsymbol.typeParams.filter(_.name.toString == name).head
          case s.Signature.TermParameter => lsym.gsymbol.paramss.flatten.filter(_.name.toString == name).head
        }
        gsym.toLogical
      }
      ssym match {
        case s.Symbol.Zero => l.None
        case s.Symbol.Root => l.Package(g.rootMirror.RootPackage, g.rootMirror.RootClass)
        case s.Symbol.Empty => l.Package(g.rootMirror.EmptyPackage, g.rootMirror.EmptyPackageClass)
        case s.Symbol.Global(howner, name, hsig) => resolve(convert(howner), name, hsig)
        case s.Symbol.Local(id) => throw new ConvertException(ssym, s"implementation restriction: internal cache has no symbol associated with $ssym")
      }
    })

    def lookupOrElseUpdate(ssym: s.Symbol, lsym: => l.Symbol): l.Symbol = symCache.getOrElseUpdate(ssym, lsym)
  }
}