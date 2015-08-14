package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.{classTag, ClassTag}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.PlainFile
import scala.meta.{Mirror => MirrorApi}
import scala.meta.{Toolbox => ToolboxApi}
import scala.meta.{Proxy => ProxyApi}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.converters.{Api => ConverterApi}
import scala.meta.internal.ast.mergeTrees
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.dialects.Scala211
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}

@context(translateExceptions = false)
class Proxy[G <: ScalaGlobal](val global: G, initialDomain: Domain = Domain())
extends ConverterApi(global) with MirrorApi with ToolboxApi with ProxyApi[G] {
  indexCompilationUnits()

  // ======= SEMANTIC CONTEXT =======

  implicit lazy val c: ScalametaSemanticContext = this

  def dialect: scala.meta.dialects.Scala211.type = {
    scala.meta.dialects.Scala211
  }

  def domain: mapi.Domain = {
    currentDomain
  }

  private[meta] def desugar(term: mapi.Term): mapi.Term = {
    ???
  }

  private[meta] def tpe(term: mapi.Term): mapi.Type = {
    val mtpe = term.requireTyped()
    mtpe.require[m.Type]
  }

  private[meta] def tpe(param: mapi.Term.Param): mapi.Type.Arg = {
    val mtpe = param.name.requireTyped()
    mtpe.require[m.Type.Arg]
  }

  private[meta] def defns(ref: mapi.Ref): Seq[mapi.Member] = {
    ref.requireDenoted()
    ref match {
      case pname: m.Name => pname.toLsymbols.map(_.toMmember(pname.toGprefix))
      case m.Term.Select(_, pname) => defns(pname)
      case m.Type.Select(_, pname) => defns(pname)
      case m.Type.Project(_, pname) => defns(pname)
      case m.Type.Singleton(pref) => defns(pref)
      case m.Ctor.Ref.Select(_, pname) => defns(pname)
      case m.Ctor.Ref.Project(_, pname) => defns(pname)
      case m.Ctor.Ref.Function(pname) => defns(pname)
      case _: m.Import.Selector => ???
    }
  }

  private[meta] def members(tpe: mapi.Type): Seq[mapi.Member] = {
    val gtpe = tpe.require[m.Type].toGtype
    val gmembers = gtpe.members.filter(_ != g.rootMirror.RootPackage)
    val pmembers = gmembers.toLogical.map(_.toMmember(gtpe))
    val pfakectors = {
      val gpresym = gtpe.typeSymbol
      if (gpresym.isTrait) List(m.Ctor.Primary(Nil, m.Ctor.Name(gpresym.name.toString).withDenot(gpresym), List(List())))
      else Nil
    }
    pfakectors ++ pmembers
  }

  private[meta] def isSubType(tpe1: mapi.Type, tpe2: mapi.Type): Boolean = {
    val gtpe1 = tpe1.require[m.Type].toGtype
    val gtpe2 = tpe2.require[m.Type].toGtype
    gtpe1 <:< gtpe2
  }

  private[meta] def lub(tpes: Seq[mapi.Type]): mapi.Type = {
    val gtpes = tpes.map(_.require[m.Type].toGtype).toList
    g.lub(gtpes).toMtype
  }

  private[meta] def glb(tpes: Seq[mapi.Type]): mapi.Type = {
    val gtpes = tpes.map(_.require[m.Type].toGtype).toList
    g.glb(gtpes).toMtype
  }

  private[meta] def parents(tpe: mapi.Type): Seq[mapi.Type] = {
    val gtpe = tpe.require[m.Type].toGtype
    gtpe.directBaseTypes.map(_.toMtype)
  }

  private[meta] def widen(tpe: mapi.Type): mapi.Type = {
    val gtpe = tpe.require[m.Type].toGtype
    gtpe.widen.toMtype
  }

  private[meta] def dealias(tpe: mapi.Type): mapi.Type = {
    val gtpe = tpe.require[m.Type].toGtype
    gtpe.dealias.toMtype
  }

  private[meta] def parents(member: mapi.Member): Seq[mapi.Member] = {
    val gpre = member.require[m.Member].toGprefix
    val Seq(lsym) = member.require[m.Member].toLsymbols
    lsym.parents.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
  }

  private[meta] def children(member: mapi.Member): Seq[mapi.Member] = {
    val gpre = member.require[m.Member].toGprefix
    val Seq(lsym) = member.require[m.Member].toLsymbols
    lsym.children.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
  }

  // ======= INTERACTIVE CONTEXT =======

  private[meta] def load(artifact: mapi.Artifact): mapi.Artifact = {
    ???
  }

  // ============== PROXY ==============

  private[meta] def toMtree[T <: mapi.Tree : ClassTag](gtree: g.Tree): T = {
    XtensionGtreeToMtree(gtree).toMtree[T]
  }

  private[meta] def toMtype(gtpe: g.Type): m.Type = {
    XtensionGtypeToMtype(gtpe).toMtype
  }

  private[meta] def toMtypeArg(gtpe: g.Type): m.Type.Arg = {
    XtensionGtypeToMtype(gtpe).toMtypeArg
  }

  private[meta] def toMmember(gsym: g.Symbol, gpre: g.Type): m.Member = {
    XtensionLsymbolToMmember(gsym.toLogical).toMmember(gpre)
  }

  private[meta] def toMannot(gannot: g.AnnotationInfo): m.Mod.Annot = {
    XtensionGannotToMannot(gannot).toMannot
  }

  private[meta] def toGtree(mtree: mapi.Tree): g.Tree = {
    XtensionMtreeToGtree(mtree.require[m.Tree]).toGtree
  }

  private[meta] def toGtype(mtpe: mapi.Type.Arg): g.Type = {
    XtensionMtypeToGtype(mtpe.require[m.Type.Arg]).toGtype
  }

  private[meta] def toGsymbols(mname: mapi.Name): Seq[g.Symbol] = {
    XtensionMnameToLsymbols(mname.require[m.Name]).toLsymbols.flatMap(_.gsymbols)
  }

  private[meta] def toGsymbols(mmember: mapi.Member): Seq[g.Symbol] = {
    XtensionMmemberToLsymbols(mmember.require[m.Member]).toLsymbols.flatMap(_.gsymbols)
  }

  // ======= INTERNAL BOOKKEEPING =======

  private var currentDomain: Domain = _

  private[meta] def indexCompilationUnits(): Unit = {
    def fail(reason: String) = throw new InfrastructureException("can't initialize a semantic proxy: " + reason)
    if (!global.isPastTyper) fail("unsupported compilation phase " + global.globalPhase + " (only phases after typer are supported)")

    // TODO: Do something smarter when assigning the initial domain, e.g.:
    // 1) Compute dependencies from settings.classpath
    // 2) Figure out resources somehow
    val globalSources = global.currentRun.units.map(unit => {
      val source = unit.body.metadata.getOrElseUpdate("scalameta", {
        // NOTE: We don't have to persist perfect trees, because tokens are transient anyway.
        // Therefore, if noone uses perfect trees in a compiler plugin, then we can avoid merging altogether.
        // Alternatively, if we hardcode merging into the core of scalameta/scalameta
        // (e.g. by making it lazy, coinciding with the first traversal of the perfect tree),
        // then we can keep mergeTrees and expose its results only to those who need perfectTrees
        // (e.g. to compiler plugins that want to work with scala.meta trees).
        // TODO: For now, I'm going to keep mergeTrees here, but in the 0.1 release,
        // we might want to turn merging off if it turns out being a big performance hit.
        // NOTE: In fact, it's more complicated than that. When we index the converted trees
        // (i.e. we add them to lsymToMmemberCache), it'd make sense to work with resugared trees,
        // because that's what users ultimately want to see when they do `t"...".members` or something.
        // So, it seems that it's still necessary to eagerly merge the trees, so that we can index them correctly.
        val syntacticTree = {
          val content = unit.source match {
            // NOTE: We need this hackaround because BatchSourceFile distorts the source code
            // by appending newlines as it sees fit. This is going to become a problem wrt TASTY,
            // because we write a sourcecode hash when serializing TASTY and verify it when deserializing.
            //
            // This is going to work just fine with real-world batchsourcefiles, but if someone creates
            // a batchsourcefile from a virtual file without a newline at the end and then will expect
            // its hash to match some other file without a newline at the end, they're in for a treat.
            //
            // Let's see whether this is going to become a problem. If it is, we'll have to distort
            // the sources everywhere in scala.meta where we compute hashes. It's so ugly that I can't
            // bring myself to doing it right now.
            case batch: BatchSourceFile if batch.file.isInstanceOf[PlainFile] => batch.file.toCharArray
            case other => other.content
          }
          content.parse[mapi.Source].require[m.Source]
        }
        val semanticTree = unit.body.toMtree[m.Source]
        val perfectTree = mergeTrees(syntacticTree, semanticTree)
        perfectTree
      })
      indexAll(source)
    }).toList
    currentDomain = Domain(Artifact(globalSources, Nil, Nil))

    // NOTE: This is necessary for semantic APIs to work correctly,
    // because otherwise the underlying global isn't going to have its symtab populated.
    // It would probably be possible to avoid this by creating completers that
    // lazily read scala.meta trees and then lazily convert them to symtab entries,
    // but that's way beyond our technological level at the moment.
    val domainClasspath = initialDomain.artifacts.flatMap(_.binaries).map(p => new java.io.File(p.path).toURI.toURL)
    val domainSources = initialDomain.artifacts.flatMap(_.sources)
    global.extendCompilerClassPath(domainClasspath: _*)
    domainSources.foreach(source => indexAll(source.require[m.Source]))
    currentDomain = Domain(initialDomain.artifacts ++ currentDomain.artifacts: _*)
  }
}