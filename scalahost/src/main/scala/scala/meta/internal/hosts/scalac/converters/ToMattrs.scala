package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.language.implicitConversions
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.reflect.internal.Flags._
import scala.meta.internal.hosts.scalac.reflect._

// This module provides functionality for scala.reflect -> scala.meta conversions
// to keep track of scala.reflect attributes.
// We have to work hard in order to provide a DSL that allows us to use .withMattrs with scala.reflect artifacts,
// but the end result is worth it, I think.
trait ToMattrs extends ReflectToolkit with MetaToolkit {
  self: Api =>

  // ======= DEFINITION OF THE DSL =======

  protected implicit class XtensionYesDenotNoTypingTree[T <: m.Tree : YesDenotNoTyping](mtree: T) {
    def withMattrs(denotlike: DenotLike): T = {
      val denot = denotlike.sdenot
      require(denot != s.Denotation.Zero)
      implicitly[YesDenotNoTyping[T]].withMattrs(mtree, denot)
    }
    def withMattrs(gpre: g.Type, symlike: SymLike): T = {
      withMattrs(l.Denotation(gpre, symlike.lsym))
    }
    def tryMattrs(denotlike: DenotLike): T = {
      val denot = denotlike.sdenot
      if (denot != s.Denotation.Zero) withMattrs(denot)
      else mtree
    }
    def tryMattrs(gpre: g.Type, symlike: SymLike): T = {
      tryMattrs(l.Denotation(gpre, symlike.lsym))
    }
  }

  protected implicit class XtensionNoDenotYesTypingTree[T <: m.Tree : NoDenotYesTyping](mtree: T) {
    def withMattrs(tpelike: TypingLike): T = {
      val typing = tpelike.styping
      require(typing != s.Typing.Zero)
      implicitly[NoDenotYesTyping[T]].withMattrs(mtree, typing)
    }
    def tryMattrs(tpelike: TypingLike)(implicit ev: NoDenotYesTyping[T]): T = {
      val typing = tpelike.styping
      if (typing != s.Typing.Zero) withMattrs(typing)
      else mtree
    }
  }

  protected implicit class XtensionYesDenotYesTypingTree[T <: m.Tree : YesDenotYesTyping](mtree: T) {
    def withMattrs(denotlike: DenotLike): T = {
      val denot @ s.Denotation.Single(spre, ssym) = denotlike.sdenot
      val gpre = spre match { case s.Prefix.Zero => g.NoPrefix; case s.Prefix.Type(mtpe: m.Type.Arg) => mtpe.toGtype }
      val gsym = symbolTable.convert(ssym).gsymbol
      val typing = self.typing(gsym.typeSignatureIn(gpre))
      withMattrs(denot, typing)
    }
    def withMattrs(gpre: g.Type, symlike: SymLike): T = {
      withMattrs(l.Denotation(gpre, symlike.lsym))
    }
    def tryMattrs(denotlike: DenotLike): T = {
      val denot = denotlike.sdenot
      if (denot != s.Denotation.Zero) withMattrs(denot)
      else mtree
    }
    def tryMattrs(gpre: g.Type, symlike: SymLike): T = {
      tryMattrs(l.Denotation(gpre, symlike.lsym))
    }

    def withMattrs(denotlike: DenotLike, tpelike: TypingLike): T = {
      val denot = denotlike.sdenot
      val typing = tpelike.styping
      require(denot != s.Denotation.Zero && typing != s.Typing.Zero)
      implicitly[YesDenotYesTyping[T]].withMattrs(mtree, denot, typing)
    }
    def withMattrs(gpre: g.Type, symlike: SymLike, tpelike: TypingLike): T = {
      withMattrs(l.Denotation(gpre, symlike.lsym), tpelike)
    }
    def tryMattrs(denotlike: DenotLike, tpelike: TypingLike): T = {
      val denot = denotlike.sdenot
      val typing = tpelike.styping
      if (denot != s.Denotation.Zero && typing != s.Typing.Zero) withMattrs(denot, typing)
      else if (denot == s.Denotation.Zero && typing == s.Typing.Zero) mtree
      else abort(debug(denot, typing))
    }
    def tryMattrs(gpre: g.Type, symlike: SymLike, tpelike: TypingLike): T = {
      tryMattrs(l.Denotation(gpre, symlike.lsym), tpelike)
    }
  }

  // ======= IMPLEMENTATION OF THE DSL =======

  private def denot(gpre0: g.Type, lsym: l.Symbol): s.Denotation = {
    if (lsym == l.Zero) s.Denotation.Zero
    else {
      require(gpre0 != g.NoType)
      val gpre = if (gpre0 == g.DefaultPrefix) lsym.gsymbol.prefix else gpre0
      val hpre = {
        if (gpre == g.NoPrefix) s.Prefix.Zero
        else s.Prefix.Type(gpre.toMtype)
      }
      val ssym = symbolTable.convert(lsym)
      s.Denotation.Single(hpre, ssym)
    }
  }

  private def typing(gtpe: g.Type): s.Typing = {
    // NOTE: s.Typing.Nonrecursive is lazy, so we need to make sure
    // that we're at the right phase when running this code
    if (gtpe == null || gtpe == g.NoType) s.Typing.Zero
    else if (gtpe.typeSymbol.isModuleClass) s.Typing.Recursive
    else s.Typing.Nonrecursive(g.enteringTyper(gtpe.toMtypeArg))
  }

  // ======= TYPE CLASSES AND IMPLICIT CONVERSIONS =======

  protected trait YesDenotNoTyping[T <: Tree] {
    def withMattrs(tree: T, denot: s.Denotation): T
  }
  protected object YesDenotNoTyping {
    implicit def Name[T <: mapi.Name]: YesDenotNoTyping[T] = new YesDenotNoTyping[T] {
      def withMattrs(tree: T, denot: s.Denotation): T = tree.require[m.Name].withAttrs(denot).asInstanceOf[T]
    }
    implicit def Ambig1[T <: mapi.Term]: YesDenotNoTyping[T] = ???
    implicit def Ambig2[T <: mapi.Term]: YesDenotNoTyping[T] = ???
  }

  protected trait NoDenotYesTyping[T <: Tree] {
    def withMattrs(tree: T, typing: s.Typing): T
  }
  protected object NoDenotYesTyping {
    implicit def Term[T <: mapi.Term]: NoDenotYesTyping[T] = new NoDenotYesTyping[T] {
      def withMattrs(tree: T, typing: s.Typing): T = tree.require[m.Term].withAttrs(typing).asInstanceOf[T]
    }
    implicit def TermParam[T <: mapi.Term.Param]: NoDenotYesTyping[T] = new NoDenotYesTyping[T] {
      // NOTE: Here we cast the tree to Term.Param.Api, not to Term.Param,
      // because `withTyping` is actually not a method on Term.Param,
      // but is instead pimped onto it via an implicit conversion from Term.Param to Term.Param.Api.
      // Typically this works well, but here we have another `withTyping` extension method
      // and things go awry.
      // TODO: Now the question is why I separated XXX and XXX.Api,
      // and why on Earth I decided that there should be an implicit conversion between them.
      // We should really revise the @ast codegen before the 0.1 release.
      // TODO: Another thing that I'd like to change in the @ast codegen is ThisType.
      // It looks like it's not really necessary, but it only creates complications.
      // We can remove it from everywhere, and just generate overriding methods
      // for places where there'll be a loss of static safety.
      def withMattrs(tree: T, typing: s.Typing): T = tree.require[m.Term.Param.Api].withAttrs(typing).asInstanceOf[T]
    }
    implicit def Ambig1[T <: mapi.Name]: NoDenotYesTyping[T] = ???
    implicit def Ambig2[T <: mapi.Name]: NoDenotYesTyping[T] = ???
  }

  protected trait YesDenotYesTyping[T <: Tree] {
    def withMattrs(tree: T, denot: s.Denotation, typing: s.Typing): T
  }
  protected object YesDenotYesTyping {
    implicit def TermName[T <: mapi.Term.Name]: YesDenotYesTyping[T] = new YesDenotYesTyping[T] {
      def withMattrs(tree: T, denot: s.Denotation, typing: s.Typing): T = tree.require[m.Term.Name].withAttrs(denot, typing).asInstanceOf[T]
    }
    implicit def CtorName[T <: mapi.Ctor.Name]: YesDenotYesTyping[T] = new YesDenotYesTyping[T] {
      def withMattrs(tree: T, denot: s.Denotation, typing: s.Typing): T = tree.require[m.Ctor.Name].withAttrs(denot, typing).asInstanceOf[T]
    }
  }

  protected trait DenotLike { def sdenot: s.Denotation }
  protected object DenotLike {
    implicit def ldenotIsDenotLike(ldenot: l.Denotation): DenotLike = new DenotLike { def sdenot = denot(ldenot.pre, ldenot.sym) }
    implicit def sdenotIsDenotLike(sdenot0: s.Denotation): DenotLike = new DenotLike { def sdenot = sdenot0 }
  }

  protected trait SymLike { def lsym: l.Symbol }
  protected object SymLike {
    implicit def gsymIsSymLike(gsym: g.Symbol): SymLike = new SymLike { def lsym = gsym.toLogical }
    implicit def lsymIsSymLike(lsym0: l.Symbol): SymLike = new SymLike { def lsym = lsym0 }
  }

  protected trait TypingLike { def styping: s.Typing }
  protected object TypingLike {
    implicit def gtpeIsTypingLike(gtpe: g.Type): TypingLike = new TypingLike { def styping = typing(gtpe) }
    implicit def stypingIsTypingLike(styping0: s.Typing): TypingLike = new TypingLike { def styping = styping0 }
  }
}