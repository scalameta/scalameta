package scala.meta
package internal.hosts.scalac

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.tools.nsc.Phase
import scala.tools.nsc.{Global => NscGlobal}
import scala.tools.asm
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.opt.LocalOpt
import scala.tools.nsc.backend.jvm._
import scala.meta.internal.tasty._
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.{ast => m}
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.debug._

// NOTE: mostly copy/pasted from https://github.com/VladimirNik/tasty/blob/7b45111d066ddbc43d859c9f6c0a81978111cf90/plugin/src/main/scala/scala/tasty/internal/scalac/Plugin.scala
abstract class ScalahostGenBCode(override val global: NscGlobal) extends scala.tools.nsc.Global$genBCode$(global) with GlobalToolkit {
  import global._
  import definitions._

  import bTypes._
  import coreBTypes._

  override val phaseName = "jvm"

  override def newPhase(prev: Phase) = new ScalahostBCodePhase(prev)

  final class PlainClassBuilder(cunit: CompilationUnit) extends SyncAndTryBuilder(cunit)

  class ScalahostBCodePhase(prev: Phase) extends BCodePhase(prev) {

    override def name = phaseName
    override def description = "Generate bytecode from ASTs using the ASM library"
    override def erasedTypes = true

    private var bytecodeWriter  : BytecodeWriter   = null
    private var mirrorCodeGen   : JMirrorBuilder   = null
    private var beanInfoCodeGen : JBeanInfoBuilder = null

    /* ---------------- q1 ---------------- */

    // case class Item1(arrivalPos: Int, cd: ClassDef, cunit: CompilationUnit) {
    //   def isPoison = { arrivalPos == Int.MaxValue }
    // }
    private val poison1 = Item1(Int.MaxValue, null, null)
    private val q1 = new java.util.LinkedList[Item1]

    /* ---------------- q2 ---------------- */

    // case class Item2(arrivalPos:   Int,
    //                  mirror:       asm.tree.ClassNode,
    //                  plain:        asm.tree.ClassNode,
    //                  bean:         asm.tree.ClassNode,
    //                  outFolder:    scala.tools.nsc.io.AbstractFile) {
    //   def isPoison = { arrivalPos == Int.MaxValue }
    // }

    private val poison2 = Item2(Int.MaxValue, null, null, null, null)
    private val q2 = new _root_.java.util.LinkedList[Item2]

    /* ---------------- q3 ---------------- */

    /*
     *  An item of queue-3 (the last queue before serializing to disk) contains three of these
     *  (one for each of mirror, plain, and bean classes).
     *
     *  @param jclassName  internal name of the class
     *  @param jclassBytes bytecode emitted for the class SubItem3 represents
     */
    // case class SubItem3(
    //   jclassName:  String,
    //   jclassBytes: Array[Byte]
    // )

    // case class Item3(arrivalPos: Int,
    //                  mirror:     SubItem3,
    //                  plain:      SubItem3,
    //                  bean:       SubItem3,
    //                  outFolder:  scala.tools.nsc.io.AbstractFile) {

    //   def isPoison  = { arrivalPos == Int.MaxValue }
    // }
    private val i3comparator = new java.util.Comparator[Item3] {
      override def compare(a: Item3, b: Item3) = {
        if (a.arrivalPos < b.arrivalPos) -1
        else if (a.arrivalPos == b.arrivalPos) 0
        else 1
      }
    }
    private val poison3 = Item3(Int.MaxValue, null, null, null, null)
    private val q3 = new java.util.PriorityQueue[Item3](1000, i3comparator)

    /*
     *  Pipeline that takes ClassDefs from queue-1, lowers them into an intermediate form, placing them on queue-2
     */
    class Worker1(needsOutFolder: Boolean) {

      val caseInsensitively = mutable.Map.empty[String, Symbol]

      def run() {
        while (true) {
          val item = q1.poll
          if (item.isPoison) {
            q2 add poison2
            return
          }
          else {
            try   { withCurrentUnit(item.cunit)(visit(item)) }
            catch {
              case ex: Throwable =>
                ex.printStackTrace()
                error(s"Error while emitting ${item.cunit.source}\n${ex.getMessage}")
            }
          }
        }
      }

      /*
       *  Checks for duplicate internal names case-insensitively,
       *  builds ASM ClassNodes for mirror, plain, and bean classes;
       *  enqueues them in queue-2.
       *
       */
      def visit(item: Item1) {
        val Item1(arrivalPos, cd, cunit) = item
        val claszSymbol = cd.symbol

        // GenASM checks this before classfiles are emitted, https://github.com/scala/scala/commit/e4d1d930693ac75d8eb64c2c3c69f2fc22bec739
        val lowercaseJavaClassName = claszSymbol.javaClassName.toLowerCase
        caseInsensitively.get(lowercaseJavaClassName) match {
          case None =>
            caseInsensitively.put(lowercaseJavaClassName, claszSymbol)
          case Some(dupClassSym) =>
            reporter.warning(
              claszSymbol.pos,
              s"Class ${claszSymbol.javaClassName} differs only in case from ${dupClassSym.javaClassName}. " +
              "Such classes will overwrite one another on case-insensitive filesystems."
            )
        }

        // -------------- mirror class, if needed --------------
        val mirrorC =
          if (isTopLevelModuleClass(claszSymbol)) {
            if (claszSymbol.companionClass == NoSymbol) {
              mirrorCodeGen.genMirrorClass(claszSymbol, cunit)
            } else {
              log(s"No mirror class for module with linked class: ${claszSymbol.fullName}")
              null
            }
          } else null

        // -------------- "plain" class --------------
        val pcb = new PlainClassBuilder(cunit)
        pcb.genPlainClass(cd)
        val outF = if (needsOutFolder) getOutFolder(claszSymbol, pcb.thisName, cunit) else null;
        val plainC = pcb.cnode

        // -------------- TASTY attr --------------
        import scala.tools.asm.CustomAttr
        if (claszSymbol.isClass) // @DarkDimius is this test needed here?
          for (meta <- cunit.body.metadata.get("scalameta").map(_.require[m.Source])) {
            enteringTyper {
              if (claszSymbol.owner.isPackageClass) {
                def fail(message: String) = abort(s"internal error when serializing TASTY: $message")
                def relevantPart[T <: m.Tree](tree: T, chain: List[g.Symbol]): T = {
                  object loop {
                    private def correlatePackage(tree: m.Pkg, chain: List[g.Symbol]): Option[m.Pkg] = {
                      def loop(ref: m.Term.Ref, chain: List[g.Symbol]): Option[List[g.Symbol]] = {
                        chain.headOption.flatMap(symbol => {
                          ref match {
                            case m.Term.Name(name) =>
                              val matches = symbol.isPackageClass && symbol.name.decoded == name
                              if (matches) Some(chain.tail) else None
                            case m.Term.Select(qual: m.Term.Ref, name) =>
                              val chain1 = loop(qual, chain)
                              chain1.flatMap(chain1 => loop(name, chain1))
                            case _ =>
                              unreachable(debug(ref, ref.show[Structure]))
                          }
                        })
                      }
                      loop(tree.ref, chain).flatMap(chain => {
                        val stats1 = apply(tree.stats, chain)
                        if (stats1.nonEmpty) Some(tree.copy(stats = stats1.require[Seq[m.Stat]])) else None
                      })
                    }
                    private def correlateLeaf(tree: m.Member, name: m.Name, chain: List[g.Symbol]): Option[m.Member] = {
                      chain.headOption.flatMap(symbol => {
                        val matchesType = {
                          if (tree.isInstanceOf[m.Defn.Class]) symbol.isClass && !symbol.isTrait && !symbol.isModuleClass
                          else if (tree.isInstanceOf[m.Defn.Trait]) symbol.isTrait
                          else if (tree.isInstanceOf[m.Defn.Object]) symbol.isModuleClass && !symbol.isPackageClass
                          else if (tree.isInstanceOf[m.Pkg.Object]) symbol.isPackageClass
                          else unreachable(debug(tree, showRaw(symbol)))
                        }
                        val matchesName = symbol.name.decoded == name.value
                        val matchesRest = chain.tail match {
                          case symbol :: Nil => tree.isInstanceOf[m.Pkg.Object] && symbol.isPackageObjectClass
                          case Nil => !tree.isInstanceOf[m.Pkg.Object]
                          case _ => false
                        }
                        if (matchesType && matchesName && matchesRest) Some(tree) else None
                      })
                    }
                    def apply(tree: m.Tree, chain: List[g.Symbol]): Option[m.Tree] = {
                      tree match {
                        case tree @ m.Source(stats) =>
                          val stats1 = apply(stats, chain)
                          if (stats1.nonEmpty) Some(tree.copy(stats = stats1.require[Seq[m.Stat]])) else None
                        case tree: m.Pkg =>
                          correlatePackage(tree, chain)
                        case tree: m.Defn.Class =>
                          correlateLeaf(tree, tree.name, chain)
                        case tree: m.Defn.Trait =>
                          correlateLeaf(tree, tree.name, chain)
                        case tree: m.Defn.Object =>
                          correlateLeaf(tree, tree.name, chain)
                        case tree: m.Pkg.Object =>
                          correlateLeaf(tree, tree.name, chain)
                        case _ =>
                          abort(s"couldn't find representation for $claszSymbol")
                      }
                    }
                    def apply(mtrees: Seq[m.Tree], chain: List[g.Symbol]): Seq[m.Tree] = {
                      mtrees.flatMap(apply(_, chain))
                    }
                  }
                  val relevantSubchain = chain.filter(!_.isEffectiveRoot)
                  val slice = loop(tree, relevantSubchain)
                  slice match {
                    case Some(slice) => slice.setTypechecked.asInstanceOf[T]
                    case None => fail(s"couldn't find representation for $claszSymbol")
                  }
                }
                val slice = relevantPart(meta, claszSymbol.ownerChain.reverse)
                Debug.logTasty({
                  val headline = claszSymbol.fullName + " (" + claszSymbol.toString + ")"
                  println("======= " + headline + " =======")
                  println(slice)
                  println(slice.show[Structure])
                })
                val dataAttr = new CustomAttr("TASTY", toTasty(meta.syntacticDigest, slice))
                plainC.visitAttribute(dataAttr)
              }
            }
          }

        // -------------- bean info class, if needed --------------
        val beanC =
          if (claszSymbol hasAnnotation BeanInfoAttr) {
            beanInfoCodeGen.genBeanInfoClass(
              claszSymbol, cunit,
              fieldSymbols(claszSymbol),
              methodSymbols(cd)
            )
          } else null

          // ----------- hand over to pipeline-2

        val item2 =
          Item2(arrivalPos,
                mirrorC, plainC, beanC,
                outF)

        q2 add item2 // at the very end of this method so that no Worker2 thread starts mutating before we're done.

      } // end of method visit(Item1)

    } // end of class BCodePhase.Worker1

    /*
     *  Pipeline that takes ClassNodes from queue-2. The unit of work depends on the optimization level:
     *
     *    (a) no optimization involves:
     *          - converting the plain ClassNode to byte array and placing it on queue-3
     */
    class Worker2 {
      def runGlobalOptimizations(): Unit = {
        import scala.collection.convert.decorateAsScala._
        q2.asScala foreach {
          case Item2(_, _, plain, _, _) =>
            // skip mirror / bean: wd don't inline into tem, and they are not used in the plain class
            if (plain != null) callGraph.addClass(plain)
        }
        bTypes.inliner.runInliner()
      }

      def localOptimizations(classNode: ClassNode): Unit = {
        BackendStats.timed(BackendStats.methodOptTimer)(localOpt.methodOptimizations(classNode))
      }

      def run() {
        if (settings.YoptInlinerEnabled) runGlobalOptimizations()

        while (true) {
          val item = q2.poll
          if (item.isPoison) {
            q3 add poison3
            return
          }
          else {
            try {
              localOptimizations(item.plain)
              addToQ3(item)
          } catch {
              case ex: Throwable =>
                ex.printStackTrace()
                error(s"Error while emitting ${item.plain.name}\n${ex.getMessage}")
            }
          }
        }
      }

      private def addToQ3(item: Item2) {

        def getByteArray(cn: asm.tree.ClassNode): Array[Byte] = {
          val cw = new CClassWriter(extraProc)
          cn.accept(cw)
          cw.toByteArray
        }

        val Item2(arrivalPos, mirror, plain, bean, outFolder) = item

        val mirrorC = if (mirror == null) null else SubItem3(mirror.name, getByteArray(mirror))
        val plainC  = SubItem3(plain.name, getByteArray(plain))
        val beanC   = if (bean == null)   null else SubItem3(bean.name, getByteArray(bean))

        if (AsmUtils.traceSerializedClassEnabled && plain.name.contains(AsmUtils.traceSerializedClassPattern)) {
          if (mirrorC != null) AsmUtils.traceClass(mirrorC.jclassBytes)
          AsmUtils.traceClass(plainC.jclassBytes)
          if (beanC != null) AsmUtils.traceClass(beanC.jclassBytes)
        }

        q3 add Item3(arrivalPos, mirrorC, plainC, beanC, outFolder)

      }

    } // end of class BCodePhase.Worker2

    // var arrivalPos = 0

    /**
     * The `run` method is overridden because the backend has a different data flow than the default
     * phase: the backend does not transform compilation units one by one, but on all units in the
     * same run. This allows cross-unit optimizations and running some stages of the backend
     * concurrently on multiple units.
     *
     *  A run of the BCodePhase phase comprises:
     *
     *    (a) set-up steps (most notably supporting maps in `BCodeTypes`,
     *        but also "the" writer where class files in byte-array form go)
     *
     *    (b) building of ASM ClassNodes, their optimization and serialization.
     *
     *    (c) tear down (closing the classfile-writer and clearing maps)
     *
     */
    override def run() {
      val bcodeStart = Statistics.startTimer(BackendStats.bcodeTimer)

      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      arrivalPos = 0 // just in case
      scalaPrimitives.init()
      bTypes.initializeCoreBTypes()
      bTypes.javaDefinedClasses.clear()
      bTypes.javaDefinedClasses ++= currentRun.symSource collect {
        case (sym, _) if sym.isJavaDefined => sym.javaBinaryName.toString
      }
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)

      // initBytecodeWriter invokes fullName, thus we have to run it before the typer-dependent thread is activated.
      bytecodeWriter  = initBytecodeWriter(cleanup.getEntryPoints)
      mirrorCodeGen   = new JMirrorBuilder
      beanInfoCodeGen = new JBeanInfoBuilder

      val needsOutfileForSymbol = bytecodeWriter.isInstanceOf[ClassBytecodeWriter]
      buildAndSendToDisk(needsOutfileForSymbol)

      // closing output files.
      bytecodeWriter.close()
      Statistics.stopTimer(BackendStats.bcodeTimer, bcodeStart)

      /* TODO Bytecode can be verified (now that all classfiles have been written to disk)
       *
       * (1) asm.util.CheckAdapter.verify()
       *       public static void verify(ClassReader cr, ClassLoader loader, boolean dump, PrintWriter pw)
       *     passing a custom ClassLoader to verify inter-dependent classes.
       *     Alternatively,
       *       - an offline-bytecode verifier could be used (e.g. Maxine brings one as separate tool).
       *       - -Xverify:all
       *
       * (2) if requested, check-java-signatures, over and beyond the syntactic checks in `getGenericSignature()`
       *
       */
    }

    /*
     *  Sequentially:
     *    (a) place all ClassDefs in queue-1
     *    (b) dequeue one at a time from queue-1, convert it to ASM ClassNode, place in queue-2
     *    (c) dequeue one at a time from queue-2, convert it to byte-array,    place in queue-3
     *    (d) serialize to disk by draining queue-3.
     */
    private def buildAndSendToDisk(needsOutFolder: Boolean) {

      feedPipeline1()
      val genStart = Statistics.startTimer(BackendStats.bcodeGenStat)
      (new Worker1(needsOutFolder)).run()
      Statistics.stopTimer(BackendStats.bcodeGenStat, genStart)

      (new Worker2).run()

      val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
      drainQ3()
      Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)

    }

    /* Feed pipeline-1: place all ClassDefs on q1, recording their arrival position. */
    private def feedPipeline1() {
      super.run()
      q1 add poison1
    }

    /* Pipeline that writes classfile representations to disk. */
    private def drainQ3() {

      def sendToDisk(cfr: SubItem3, outFolder: scala.tools.nsc.io.AbstractFile) {
        if (cfr != null){
          val SubItem3(jclassName, jclassBytes) = cfr
          try {
            val outFile =
              if (outFolder == null) null
              else getFileForClassfile(outFolder, jclassName, ".class")
            bytecodeWriter.writeClass(jclassName, jclassName, jclassBytes, outFile)
          }
          catch {
            case e: FileConflictException =>
              error(s"error writing $jclassName: ${e.getMessage}")
          }
        }
      }

      var moreComing = true
      // `expected` denotes the arrivalPos whose Item3 should be serialized next
      var expected = 0

      while (moreComing) {
        val incoming = q3.poll
        moreComing   = !incoming.isPoison
        if (moreComing) {
          val item = incoming
          val outFolder = item.outFolder
          sendToDisk(item.mirror, outFolder)
          sendToDisk(item.plain,  outFolder)
          sendToDisk(item.bean,   outFolder)
          expected += 1
        }
      }

      // we're done
      assert(q1.isEmpty, s"Some ClassDefs remained in the first queue: $q1")
      assert(q2.isEmpty, s"Some classfiles remained in the second queue: $q2")
      assert(q3.isEmpty, s"Some classfiles weren't written to disk: $q3")

    }

    override def apply(cunit: CompilationUnit): Unit = {

      def gen(tree: Tree) {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case cd: ClassDef         =>
            q1 add Item1(arrivalPos, cd, cunit)
            arrivalPos += 1
        }
      }

      gen(cunit.body)
    }

  } // end of class BCodePhase

} // end of class GenBCode
