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
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.reflect.internal.NoPhase
import scala.reflect.internal.Phase
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.PlainFile
import scala.meta.artifacts.Artifact.Adhoc
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.{Context => ContextApi}
import scala.meta.internal.hosts.scalac.{Adapter => AdapterApi}
import scala.meta.internal.hosts.scalac.converters.{Api => ConverterApi}
import scala.meta.internal.ast.mergeTrees
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.plugins.Plugin
import scala.meta.dialects.Scala211
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.flags._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.hosts.scalac.{Plugin => ScalahostPlugin}

@context(translateExceptions = false)
class Adapter[G <: ScalaGlobal](val global: G, initialDomain: Domain = Domain())
extends ConverterApi(global) with ContextApi with AdapterApi[G] {
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
    if (tree.isTypechecked) return tree
    def loop(tree: m.Tree): m.Tree = {
      def nativeTypecheck(tree: g.Tree, mode: Mode): Either[String, g.Tree] = {
        import global._
        import definitions._
        import analyzer._
        val ownerClass = rootMirror.EmptyPackageClass.newClassSymbol(newTypeName("<ScalahostTypecheck>"))
        build.setInfo(ownerClass, ClassInfoType(List(ObjectTpe), newScope, ownerClass))
        val owner = ownerClass.newLocalDummy(NoPosition)
        val typer = newTyper(rootContext(NoCompilationUnit, EmptyTree).make(tree, owner))
        typer.context.initRootContext() // need to manually set context mode, otherwise typer.silent will throw exceptions
        enteringTyper({
          val old = settings.exposeEmptyPackage.value
          try {
            if (!tree.isInstanceOf[g.PackageDef]) settings.exposeEmptyPackage.value = true
            typer.silent(_.typed(tree, mode, WildcardType), reportAmbiguousErrors = false) match {
              case SilentResultValue(result) => Right(result)
              case error @ SilentTypeError(_) => Left(error.err.errMsg)
            }
          } finally {
            settings.exposeEmptyPackage.value = old
          }
        })
      }
      def convertingTypecheck(tree: m.Tree, mode: Mode): m.Tree = {
        import conversions._
        nativeTypecheck(tree.toGtree, mode) match {
          case Right(gtree1) => gtree1.toMeta.require[m.Tree]
          case Left(error) => throw new TypecheckException(tree, error)
        }
      }
      tree.parent match {
        case Some(parent) =>
          // NOTE: This works because typechecking in scala.meta doesn't change the shape of the tree.
          val index = parent.children.indexOf(tree)
          val parent1 = loop(parent.require[m.Tree])
          parent1.children(index).require[m.Tree]
        case None =>
          val sytree = tree
          val setree = tree match {
            case tree: m.Term =>
              convertingTypecheck(tree, EXPRmode)
            case tree: m.Type =>
              convertingTypecheck(tree, TYPEmode)
            case _ =>
              // TODO: It would be nice to be able to typecheck m.Source, but I've no idea how to do that.
              // The thing is that in order to have scalac typecheck top-level definitions, we have to enter them globally,
              // and that brings troubles.
              //
              // The first trouble is the fact that we have to undo all those enters afterwards.
              // That's actually doable, because we can compute the paths to the symbols that are to be entered in advance,
              // simply by analyzing the syntactic input. Then, we could back up the signatures of the corresponding
              // scala.reflect symbols before typechecking and restore the signatures afterwards.
              //
              // The second trouble, and that one I don't yet know how to fix, is that we actually must not clean up
              // the global symbols from the symbol table right after typechecking, because then we won't be able
              // to answer semantic requests about the resulting typechecked scala.meta tree.
              //
              // This seems to be a hard-to-solve conflict of interests. On the one hand, we don't want typechecking
              // to pollute the symbol table, but on the other hand, we have to allow that, because otherwise semantic ops
              // won't work. Luckily, this is only a problem for global definitions, and it doesn't prevent us from typchecking
              // terms and types, which is what we need the most at the moment.
              val message = s"don't yet know how to typecheck ${tree.productPrefix}"
              throw new UnsupportedOperationException(s"implementation restriction: $message")
          }
          mergeTrees(sytree, setree)
      }
    }
    loop(tree.require[m.Tree])
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

  private[meta] def supermembers(untypedMember: mapi.Member): Seq[mapi.Member] = {
    val member = typecheck(untypedMember).require[m.Member]
    val gpre = member.toGprefix
    val Seq(lsym) = member.toLsymbols
    val supermembers = lsym.supermembers.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
    supermembers.map(_.forceTypechecked)
  }

  private[meta] def submembers(untypedMember: mapi.Member): Seq[mapi.Member] = {
    val member = typecheck(untypedMember).require[m.Member]
    val gpre = member.toGprefix
    val Seq(lsym) = member.toLsymbols
    val submembers = lsym.submembers.map(_.toMmember(gpre)) // TODO: also instantiate type parameters when necessary
    submembers.map(_.forceTypechecked)
  }

  private[meta] def isSubtype(untypedTpe1: mapi.Type, untypedTpe2: mapi.Type): Boolean = {
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

  private[meta] def supertypes(untypedTpe: mapi.Type): Seq[mapi.Type] = {
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

  // ======= INTERACTIVE CONTEXT =======

  private[meta] def load(artifacts: Seq[mapi.Artifact]): Seq[mapi.Artifact] = {
    Debug.logScalahost(println(s"loading ${artifacts.length} artifacts from $artifacts"))

    val artifactClasspath = artifacts.flatMap(_.binaries).map(p => new File(p.path).toURI.toURL)
    Debug.logScalahost(println(s"indexing artifact classpath: $artifactClasspath"))
    val globalClasspath = global.classPath.asURLs.toSet
    Debug.logScalahost(println(s"global classpath: ${global.classPath.asURLs}"))
    val deltaClasspath = artifactClasspath.filter(entry => !globalClasspath(entry))
    if (deltaClasspath.nonEmpty) global.extendCompilerClassPath(deltaClasspath: _*)

    val artifactSources = artifacts.flatMap(_.sources.map(_.require[m.Source]))
    val (typedSources, untypedSources) = artifactSources.partition(_.isTypechecked)
    Debug.logScalahost(println(s"indexing ${artifactSources.length} artifact sources (${typedSources.length} out of ${artifactSources.length} are TYPECHECKED)"))
    typedSources.foreach(source => {
      // TODO: looks like we really need some kind of a Source.id
      Debug.logScalahost(println(s"indexing ${source.show[Summary]}"))
      indexAll(source.require[m.Source])
    })

    Debug.logScalahost(untypedSources.foreach(source => println(s"typechecking ${source.show[Summary]}")))
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

      val m_firstPhase = classOf[Run].getDeclaredMethods().find(_.getName == "firstPhase").get
      m_firstPhase.setAccessible(true)
      val firstPhase = m_firstPhase.invoke(currentRun).asInstanceOf[Phase]
      val relevantPhases = firstPhase.iterator.takeWhile(_.id < math.max(globalPhase.id, currentRun.typerPhase.id))
      def applyPhase(ph: Phase, unit: CompilationUnit) = enteringPhase(ph)(ph.asInstanceOf[GlobalPhase].applyPhase(unit))
      relevantPhases.foreach(ph => units.foreach(applyPhase(ph, _)))

      val m_refreshProgress = classOf[Run].getDeclaredMethods().find(_.getName == "refreshProgress").get
      m_refreshProgress.setAccessible(true)
      m_refreshProgress.invoke(currentRun)

      untypedSources.zip(units).map({ case (sysource, unit) =>
        val sesource = {
          Debug.logScalahost(println(s"converting ${sysource.show[Summary]}"))
          unit.body.toMtree[m.Source]
        }
        Debug.logScalahost(println(s"merging ${sysource.show[Summary]}"))
        val mesource = mergeTrees(sysource, sesource)
        Debug.logScalahost(println(s"indexing ${sysource.show[Summary]}"))
        (sysource, indexAll(mesource))
      }).toMap
    }

    def ensureTypechecked(source: Source) = typedUntypedSources.getOrElse(source, source)
    val artifacts1 = artifacts.map {
      case artifact @ Adhoc(sources, _, _) => artifact.copy(sources = sources.map(ensureTypechecked))
      case artifact => artifact
    }

    currentDomain = Domain(currentDomain.artifacts ++ artifacts1: _*)
    artifacts1
  }

  // ============== Adapter ==============

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
      throw new InfrastructureException(s"can't initialize a semantic Adapter from $status: " + reason, ex)
    }

    Debug.logScalahost({
      println(s"initializing semantic Adapter from $global and $initialDomain")
      if (fromScratch) println("starting from scratch") else println("wrapping a pre-existing global")
    })

    try {
      // NOTE: This is necessary for semantic APIs to work correctly,
      // because otherwise the underlying global isn't going to have its symtab populated.
      // It would probably be possible to avoid this by creating completers that
      // lazily read scala.meta trees and then lazily convert them to symtab entries,
      // but that's way beyond our technological level at the moment.
      val domainClasspath = initialDomain.artifacts.flatMap(_.binaries).map(p => new File(p.path).toURI.toURL)
      Debug.logScalahost(println(s"indexing domain classpath: $domainClasspath"))
      if (fromScratch) {
        // NOTE: Make sure that the internal classpath cache hasn't been initialized yet.
        // If it has, we're in trouble, because our modifications to settings.classpath.value
        // aren't going to get propagated. Of course, I tried to sidestep this problem
        // by using something like `global.extendCompilerClassPath(domainClasspath: _*)`,
        // but unfortunately it throws an obscure assertion error, so I just gave up.
        Debug.logScalahost(println(s"setting the classpath..."))
        val m_currentClassPath = classOf[JavaPlatform].getDeclaredMethod("currentClassPath")
        m_currentClassPath.setAccessible(true)
        val currentClassPath = m_currentClassPath.invoke(global.platform).asInstanceOf[Option[_]]
        require(currentClassPath.isEmpty)
        global.settings.classpath.value = domainClasspath.map(_.getPath).mkString(File.pathSeparator)

        // NOTE: Install the scalahost plugin, because we need its analyzer hijacks.
        // In order to do that, we need to force the initialization of `Global.plugins` to hijack it afterwards.
        // Alternatively, we could try to register scalahost in compiler.settings in Compiler.scala,
        // but for that we need to know our classpath, and I don't know how to do that.
        Debug.logScalahost(println(s"ensuring the scalahost plugin..."))
        val _ = global.plugins
        if (!global.plugins.exists(_.name == "scalahost")) {
          val f_plugins = global.getClass.getDeclaredField("plugins")
          f_plugins.setAccessible(true)
          val plugins = f_plugins.get(global).asInstanceOf[List[Plugin]]
          val f_roughPluginsList = global.getClass.getDeclaredField("roughPluginsList")
          f_roughPluginsList.setAccessible(true)
          val roughPluginsList = f_roughPluginsList.get(global).asInstanceOf[List[Plugin]]
          val scalahostPlugin = new ScalahostPlugin(global)
          f_roughPluginsList.set(global, roughPluginsList :+ scalahostPlugin)
          f_plugins.set(global, plugins :+ scalahostPlugin)
        }

        // NOTE: This is mandatory, because this is going to: a) finish necessary initialization of the compiler,
        // b) force computation of the classpath that we have just set.
        Debug.logScalahost(println(s"starting the compilation pipeline from pickler..."))
        val run = new global.Run
        global.phase = run.picklerPhase
        global.globalPhase = run.picklerPhase
      } else {
        // NOTE: Probably won't work, it's a very mysterious method.
        // This scenario isn't on our immediate roadmap, so I'll just leave this line of code here
        // and accompany it by a warning comment.
        Debug.logScalahost(println(s"setting the classpath..."))
        global.extendCompilerClassPath(domainClasspath: _*)

        Debug.logScalahost(println(s"ensuring the scalahost plugin..."))
        val scalahostPlugin = global.plugins.find(_.name == "scalahost")
        if (scalahostPlugin.isEmpty) fail("the underlying compiler should have the scalahost plugin enabled", None)
      }
    } catch {
      case ex: InfrastructureException =>
        throw ex
      case ex: Exception =>
        var message = ex.getMessage
        if (ex.isInstanceOf[MissingRequirementError]) {
          message = message.stripSuffix(".")
          message += " (have you forgotten to reference the standard library"
          message += " when creating a scala.meta context?)"
        }
        fail(message, Some(ex))
    }

    Debug.logScalahost(println(s"indexing ${global.currentRun.units.toList.length} global sources"))
    val globalSources = global.currentRun.units.map(unit => {
      val unitId = unit.source.file.path
      Debug.logScalahost({
        if (unit.body.metadata.contains("scalameta")) println(s"found cached scala.meta tree for $unitId")
        else println(s"computing scala.meta tree for $unitId")
      })
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
          Debug.logScalahost(println(s"parsing $unitId"))
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
          Debug.logScalahost(println(s"converting $unitId"))
          unit.body.toMtree[m.Source]
        }
        Debug.logScalahost(println(s"merging $unitId"))
        val perfectTree = mergeTrees(syntacticTree, semanticTree)
        perfectTree
      })
      Debug.logScalahost(println(s"indexing $unitId"))
      indexAll(source)
    }).toList

    // TODO: Do something smarter when assigning the initial domain, e.g.:
    // 1) Compute dependencies from settings.classpath
    // 2) Figure out resources somehow
    val globalArtifacts = List(Artifact(globalSources, Nil, Nil))
    currentDomain = Domain(globalArtifacts: _*)

    Debug.logScalahost(println(s"indexing ${initialDomain.artifacts.length} domain artifacts"))
    load(initialDomain.artifacts.toList)

    Debug.logScalahost(println(s"initialized semantic Adapter from $global and $initialDomain"))
  }
}