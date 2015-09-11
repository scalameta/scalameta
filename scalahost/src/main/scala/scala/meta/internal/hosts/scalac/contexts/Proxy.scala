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
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.NoPhase
import scala.reflect.internal.Phase
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
  initializeFromDomain(initialDomain)

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
    if (tree.isTypechecked) return tree
    ???
  }

  private[meta] def defns(untypedRef: mapi.Ref): Seq[mapi.Member] = {
    val ref = typecheck(untypedRef).require[m.Ref]
    val result = ref match {
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
    result.map(_.forceTypechecked)
  }

  private[meta] def members(untypedTpe: mapi.Type): Seq[mapi.Member] = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val gmembers = gtpe.members.filter(_ != g.rootMirror.RootPackage)
    val mmembers = gmembers.toLogical.map(_.toMmember(gtpe))
    val mfakectors = if (gtpe.typeSymbol.isTrait) List(mfakector(gtpe)) else Nil
    val members = mfakectors ++ mmembers
    members.map(_.forceTypechecked)
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
    val lub = g.lub(gtpes).toMtype
    lub.forceTypechecked
  }

  private[meta] def glb(untypedTpes: Seq[mapi.Type]): mapi.Type = {
    val tpes = untypedTpes.map(untypedTpe => typecheck(untypedTpe).require[m.Type])
    val gtpes = tpes.map(_.toGtype).toList
    val glb = g.glb(gtpes).toMtype
    glb.forceTypechecked
  }

  private[meta] def parents(untypedTpe: mapi.Type): Seq[mapi.Type] = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val parents = gtpe.directBaseTypes.map(_.toMtype)
    parents.map(_.forceTypechecked)
  }

  private[meta] def widen(untypedTpe: mapi.Type): mapi.Type = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val widened = gtpe.widen.toMtype
    widened.forceTypechecked
  }

  private[meta] def dealias(untypedTpe: mapi.Type): mapi.Type = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val dealiased = gtpe.dealias.toMtype
    dealiased.forceTypechecked
  }

  private[meta] def parents(untypedMember: mapi.Member): Seq[mapi.Member] = {
    val member = typecheck(untypedMember).require[m.Member]
    val gpre = member.toGprefix
    val Seq(lsym) = member.toLsymbols
    val parents = lsym.parents.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
    parents.map(_.forceTypechecked)
  }

  private[meta] def children(untypedMember: mapi.Member): Seq[mapi.Member] = {
    val member = typecheck(untypedMember).require[m.Member]
    val gpre = member.toGprefix
    val Seq(lsym) = member.toLsymbols
    val children = lsym.children.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
    children.map(_.forceTypechecked)
  }

  // ======= INTERACTIVE CONTEXT =======

  private[meta] def load(artifacts: Seq[mapi.Artifact]): Seq[mapi.Artifact] = {
    if (Debug.scalahost) println(s"loading ${artifacts.length} artifacts from $artifacts")

    val artifactClasspath = artifacts.flatMap(_.binaries).map(p => new File(p.path).toURI.toURL)
    if (Debug.scalahost) println(s"indexing artifact classpath: $artifactClasspath")
    val globalClasspath = global.classPath.asURLs.toSet
    val deltaClasspath = artifactClasspath.filter(entry => !globalClasspath(entry))
    if (deltaClasspath.nonEmpty) global.extendCompilerClassPath(deltaClasspath: _*)

    val artifactSources = artifacts.flatMap(_.sources.map(_.require[m.Source]))
    val (typedSources, untypedSources) = artifactSources.partition(_.isTypechecked)
    if (Debug.scalahost) println(s"indexing ${artifactSources.length} artifact sources (${typedSources.length} out of ${artifactSources.length} are TYPECHECKED)")
    typedSources.foreach(source => {
      // TODO: looks like we really need some kind of a Source.id
      if (Debug.scalahost) println(s"indexing ${source.show[Summary]}")
      indexAll(source.require[m.Source])
    })

    if (Debug.scalahost) untypedSources.foreach(source => println(s"typechecking ${source.show[Summary]}"))
    val typedUntypedSources: Map[Source, Source] = {
      import global._
      val units = untypedSources.map(source => {
        val unit = new CompilationUnit(newSourceFile("", "<scalahost>"))
        unit.body = source.toGtree
        val m_addUnit = currentRun.getClass.getDeclaredMethods().find(_.getName.endsWith("$addUnit")).get
        m_addUnit.setAccessible(true)
        m_addUnit.invoke(currentRun, unit)
        unit
      })

      val m_firstPhase = currentRun.getClass.getDeclaredMethods().find(_.getName == "firstPhase").get
      m_firstPhase.setAccessible(true)
      val firstPhase = m_firstPhase.invoke(currentRun).asInstanceOf[Phase]
      val relevantPhases = firstPhase.iterator.takeWhile(_.id < math.max(globalPhase.id, currentRun.typerPhase.id))
      def applyPhase(ph: Phase, unit: CompilationUnit) = enteringPhase(ph)(ph.asInstanceOf[GlobalPhase].applyPhase(unit))
      relevantPhases.foreach(ph => units.foreach(applyPhase(ph, _)))

      val m_refreshProgress = currentRun.getClass.getDeclaredMethods().find(_.getName == "refreshProgress").get
      m_refreshProgress.setAccessible(true)
      m_refreshProgress.invoke(currentRun)

      untypedSources.zip(units).map({ case (sysource, unit) =>
        val sesource = {
          if (Debug.scalahost) println(s"converting ${sysource.show[Summary]}")
          unit.body.toMtree[m.Source]
        }
        if (Debug.scalahost) println(s"merging ${sysource.show[Summary]}")
        val mesource = mergeTrees(sysource, sesource)
        if (Debug.scalahost) println(s"indexing ${sysource.show[Summary]}")
        (sysource, indexAll(mesource))
      }).toMap
    }

    def ensureTypechecked(source: Source) = typedUntypedSources.getOrElse(source, source)
    val artifacts1 = artifacts.map {
      case taxonomic.Artifact.Adhoc(sources, resources, deps) => taxonomic.Artifact.Adhoc(sources.map(ensureTypechecked), resources, deps)
      case artifact: taxonomic.Artifact.Unmanaged => artifact
      case artifact: taxonomic.Artifact.Maven => artifact
    }

    currentDomain = Domain(currentDomain.artifacts ++ artifacts1: _*)
    artifacts1
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
  private def initializeFromDomain(initialDomain: Domain): Unit = withWriteOnlyIndices {
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

    if (Debug.scalahost) println(s"indexing ${global.currentRun.units.toList.length} global sources")
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
        // (i.e. we add them to ssymToMmemberCache), it'd make sense to work with resugared trees,
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
          unit.body.toMtree[m.Source]
        }
        if (Debug.scalahost) println(s"merging $unitId")
        val perfectTree = mergeTrees(syntacticTree, semanticTree)
        perfectTree
      })
      if (Debug.scalahost) println(s"indexing $unitId")
      indexAll(source)
    }).toList

    // TODO: Do something smarter when assigning the initial domain, e.g.:
    // 1) Compute dependencies from settings.classpath
    // 2) Figure out resources somehow
    val globalArtifacts = List(Artifact(globalSources, Nil, Nil))
    currentDomain = Domain(globalArtifacts: _*)

    if (Debug.scalahost) println(s"indexing ${initialDomain.artifacts.length} domain artifacts")
    val domainArtifacts1 = load(initialDomain.artifacts.toList)
    currentDomain = Domain(currentDomain.artifacts ++ domainArtifacts1: _*)

    if (Debug.scalahost) println(s"initialized semantic proxy from $global and $initialDomain")
  }
}