package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.debug._
import java.io.File
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.{classTag, ClassTag}
import scala.reflect.internal.NoPhase
import scala.reflect.internal.MissingRequirementError
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
import scala.meta.internal.flags._
import scala.meta.internal.ui.Summary

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

  private[meta] def typecheck(tree: mapi.Tree): mapi.Tree = {
    // TODO: implement this
    // 1) respect tree.parent
    // 2) don't retypecheck if that's unnecessary
    if (tree.isTypechecked) return tree
    ???
  }

  private[meta] def defns(untypedRef: mapi.Ref): Seq[mapi.Member] = {
    require(!disallowApisThatReturnMembers)
    val ref = typecheck(untypedRef).require[m.Ref]
    ref match {
      case pname: m.Name => pname.toLsymbols.map(_.toMmember(pname.toGprefix))
      case m.Term.Select(_, pname) => defns(pname)
      case m.Type.Select(_, pname) => defns(pname)
      case m.Type.Project(_, pname) => defns(pname)
      case m.Type.Singleton(pref) => defns(pref)
      case m.Pat.Type.Project(_, pname) => defns(pname)
      case m.Ctor.Ref.Select(_, pname) => defns(pname)
      case m.Ctor.Ref.Project(_, pname) => defns(pname)
      case m.Ctor.Ref.Function(pname) => defns(pname)
      case _: m.Import.Selector => ???
    }
  }

  private[meta] def members(untypedTpe: mapi.Type): Seq[mapi.Member] = {
    require(!disallowApisThatReturnMembers)
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val gmembers = gtpe.members.filter(_ != g.rootMirror.RootPackage)
    val pmembers = gmembers.toLogical.map(_.toMmember(gtpe))
    val pfakectors = {
      val gpresym = gtpe.typeSymbol
      if (gpresym.isTrait) List(m.Ctor.Primary(Nil, m.Ctor.Name(gpresym.name.toString).withDenot(gpresym), List(List())))
      else Nil
    }
    pfakectors ++ pmembers
  }

  private[meta] def isSubType(untypedTpe1: mapi.Type, untypedTpe2: mapi.Type): Boolean = {
    val tpe1 = typecheck(untypedTpe1).require[m.Type]
    val tpe2 = typecheck(untypedTpe2).require[m.Type]
    val gtpe1 = tpe1.toGtype
    val gtpe2 = tpe2.toGtype
    gtpe1 <:< gtpe2
  }

  private[meta] def lub(untypedTpes: Seq[mapi.Type]): mapi.Type = {
    val tpes = untypedTpes.map(untypedTpe => typecheck(untypedTpe).require[m.Type])
    val gtpes = tpes.map(_.toGtype).toList
    g.lub(gtpes).toMtype
  }

  private[meta] def glb(untypedTpes: Seq[mapi.Type]): mapi.Type = {
    val tpes = untypedTpes.map(untypedTpe => typecheck(untypedTpe).require[m.Type])
    val gtpes = tpes.map(_.toGtype).toList
    g.glb(gtpes).toMtype
  }

  private[meta] def parents(untypedTpe: mapi.Type): Seq[mapi.Type] = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    gtpe.directBaseTypes.map(_.toMtype)
  }

  private[meta] def widen(untypedTpe: mapi.Type): mapi.Type = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    gtpe.widen.toMtype
  }

  private[meta] def dealias(untypedTpe: mapi.Type): mapi.Type = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    gtpe.dealias.toMtype
  }

  private[meta] def parents(untypedMember: mapi.Member): Seq[mapi.Member] = {
    require(!disallowApisThatReturnMembers)
    val member = typecheck(untypedMember).require[m.Member]
    val gpre = member.toGprefix
    val Seq(lsym) = member.toLsymbols
    lsym.parents.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
  }

  private[meta] def children(untypedMember: mapi.Member): Seq[mapi.Member] = {
    require(!disallowApisThatReturnMembers)
    val member = typecheck(untypedMember).require[m.Member]
    val gpre = member.toGprefix
    val Seq(lsym) = member.toLsymbols
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
  private var disallowApisThatReturnMembers: Boolean = false

  private[meta] def indexCompilationUnits(): Unit = {
    // NOTE: We disable certain semantic operations when the compilation units are being indexed.
    // Otherwise, we could run into problems when we, say, resolve a reference during indexing
    // and the result of the resolution is an object that is reconstructed from a symbol,
    // not loaded directly from a source that hasn't been indexed yet.
    disallowApisThatReturnMembers = true

    try {
      val fromScratch = global.currentRun == null
      def fail(reason: String, ex: Option[Exception]) = {
        val status = if (fromScratch) "scratch" else "a pre-existing compiler"
        throw new InfrastructureException(s"can't initialize a semantic proxy from $status: " + reason, ex)
      }

      if (Debug.scalahost) {
        println(s"initializing semantic proxy from $global and $initialDomain")
        if (fromScratch) println("starting from scratch") else println("wrapping a pre-existing global")
      }

      try {
        // NOTE: This is necessary for semantic APIs to work correctly,
        // because otherwise the underlying global isn't going to have its symtab populated.
        // It would probably be possible to avoid this by creating completers that
        // lazily read scala.meta trees and then lazily convert them to symtab entries,
        // but that's way beyond our technological level at the moment.
        val domainClasspath = initialDomain.artifacts.flatMap(_.binaries).map(p => new File(p.path).toURI.toURL)
        if (Debug.scalahost) println(s"indexing domain classpath: $domainClasspath")
        if (fromScratch) {
          // NOTE: Make sure that the internal classpath cache hasn't been initialized yet.
          // If it has, we're in trouble, because our modifications to settings.classpath.value
          // aren't going to get propagated. Of course, I tried to sidestep this problem
          // by using something like `global.extendCompilerClassPath(domainClasspath: _*)`,
          // but unfortunately it throws an obscure assertion error, so I just gave up.
          val m_currentClassPath = global.platform.getClass.getDeclaredMethod("currentClassPath")
          m_currentClassPath.setAccessible(true)
          val currentClassPath = m_currentClassPath.invoke(global.platform).asInstanceOf[Option[_]]
          require(currentClassPath.isEmpty)
          global.settings.classpath.value = domainClasspath.map(_.getPath).mkString(File.pathSeparator)

          // NOTE: This is mandatory, because this is going to: a) finish necessary initialization of the compiler,
          // b) force computation of the classpath that we have just set.
          val run = new global.Run
          global.phase = run.picklerPhase
          global.globalPhase = run.picklerPhase
        } else {
          // NOTE: Probably won't work, it's a very mysterious method.
          // This scenario isn't on our immediate roadmap, so I'll just leave this line of code here
          // and accompany it by a warning comment.
          global.extendCompilerClassPath(domainClasspath: _*)
        }
      } catch {
        case ex: Exception =>
          var message = ex.getMessage
          if (ex.isInstanceOf[MissingRequirementError]) {
            message = message.stripSuffix(".")
            message += " (have you forgotten to reference the standard library"
            message += " when creating a mirror or a toolbox?)"
          }
          fail(message, Some(ex))
      }

      if (Debug.scalahost) println(s"indexing global sources: all ${global.currentRun.units.toList.length} of them")
      val globalSources = global.currentRun.units.map(unit => {
        val unitId = unit.source.file.path
        if (Debug.scalahost) {
          if (unit.body.metadata.contains("scalameta")) println(s"found cached scala.meta tree for $unitId")
          else println(s"computing scala.meta tree for $unitId")
        }
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
            if (Debug.scalahost) println(s"parsing $unitId")
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
          val semanticTree = {
            if (Debug.scalahost) println(s"converting $unitId")
            unit.body.toMtree[m.Source].setTypechecked
          }
          if (Debug.scalahost) println(s"merging $unitId")
          val perfectTree = mergeTrees(syntacticTree, semanticTree)
          perfectTree
        })
        if (Debug.scalahost) println(s"indexing $unitId")
        indexAll(source)
      }).toList

      val domainSources = initialDomain.artifacts.flatMap(_.sources)
      if (Debug.scalahost) println(s"indexing domain sources: all ${domainSources.length} of them")
      domainSources.foreach(source => {
        // TODO: looks like we really need some kind of a Source.id
        if (Debug.scalahost) println(s"indexing ${source.show[Summary]}")
        indexAll(source.require[m.Source])
      })

      // TODO: Do something smarter when assigning the initial domain, e.g.:
      // 1) Compute dependencies from settings.classpath
      // 2) Figure out resources somehow
      if (Debug.scalahost) println(s"merging global and initial domains")
      val globalArtifacts = List(Artifact(globalSources, Nil, Nil))
      val domainArtifacts = initialDomain.artifacts
      currentDomain = Domain(globalArtifacts ++ domainArtifacts: _*)

      println(s"initialized semantic proxy from $global and $initialDomain")
    } finally {
      disallowApisThatReturnMembers = false
    }
  }
}