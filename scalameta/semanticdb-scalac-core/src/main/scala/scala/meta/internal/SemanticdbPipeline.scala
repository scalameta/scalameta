package scala.meta.internal

import java.io._
import java.net.URI
import scala.compat.Platform.EOL
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.util.control.NonFatal
import scala.{meta => m}
import scala.meta.io._
import scala.meta.internal.semanticdb.DatabaseOps
import scala.meta.internal.semanticdb.{vfs => v}

trait SemanticdbPipeline extends DatabaseOps { self: SemanticdbPlugin =>
  lazy val scalametaTargetroot = AbsolutePath(
    new File(
      global.settings.outputDirs.getSingleOutput
        .map(_.file.getAbsolutePath)
        .getOrElse(global.settings.d.value)))
  implicit class XtensionURI(uri: URI) { def toFile: File = new File(uri) }
  implicit class XtensionUnit(unit: g.CompilationUnit) {
    def isIgnored: Boolean = {
      config.mode.isDisabled ||
      !unit.source.file.name.endsWith(".scala") || {
        val fullName = unit.source.file.file.getAbsolutePath
        !config.fileFilter.matches(fullName)
      }
    }
  }

  def handleError(unit: g.CompilationUnit): PartialFunction[Throwable, Unit] = {
    case NonFatal(ex) =>
      val writer = new StringWriter()
      val path = unit.source.file.path
      writer.write(s"failed to generate semanticdb for $path:$EOL")
      ex.printStackTrace(new PrintWriter(writer))
      val msg = writer.toString
      import scala.meta.internal.semanticdb.FailureMode._
      config.failures match {
        case Error => global.reporter.error(g.NoPosition, msg)
        case Warning => global.reporter.warning(g.NoPosition, msg)
        case Info => global.reporter.info(g.NoPosition, msg, force = true)
        case Ignore => // do nothing.
      }
  }

  object SemanticdbTyperComponent extends PluginComponent {
    val global: SemanticdbPipeline.this.global.type = SemanticdbPipeline.this.global
    val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "semanticdb-typer"
    override val description = "computes and persists semanticdb after typer"
    def newPhase(_prev: Phase) = new ComputeSemanticdbPhase(_prev)
    class ComputeSemanticdbPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        try {
          if (unit.isIgnored) return
          validateCompilerState()
          val mdoc = unit.toDocument
          val mdb = m.Database(List(mdoc))
          mdb.save(scalametaTargetroot, config.sourceroot)
        } catch handleError(unit)
      }

      override def run(): Unit = {
        timestampComputeStarted = System.nanoTime()
        super.run()
        timestampComputeFinished = System.nanoTime()
        symbolCache.clear()
      }
    }
  }

  object SemanticdbJvmComponent extends PluginComponent {
    val global: SemanticdbPipeline.this.global.type = SemanticdbPipeline.this.global
    val runsAfter = List("jvm")
    override val runsRightAfter = Some("jvm")
    val phaseName = "semanticdb-jvm"
    override val description =
      "compute and persist additional semanticdb entries created after typer"
    def newPhase(_prev: Phase) = new PersistSemanticdbPhase(_prev)
    class PersistSemanticdbPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        if (unit.isIgnored) return
        try {
          if (config.messages.saveMessages) {
            val messages = unit.reportedMessages(Map.empty)
            if (messages.nonEmpty) {
              val mdoc = m.Document(
                input = m.Input.File(unit.source.file.file),
                language = language,
                names = Nil,
                messages = messages,
                symbols = Nil,
                synthetics = Nil
              )
              val mdb = m.Database(mdoc :: Nil)
              mdb.append(scalametaTargetroot, config.sourceroot)
            }
          }
        } catch handleError(unit)
      }

      override def run(): Unit = {
        timestampPersistStarted = System.nanoTime()
        val vdb = v.Database.load(Classpath(scalametaTargetroot))
        val orphanedVentries = vdb.entries.filter(ventry => {
          val scalaName = v.SemanticdbPaths.toScala(ventry.fragment.name)
          !config.sourceroot.resolve(scalaName).isFile
        })
        orphanedVentries.map(ve => {
          def cleanupUpwards(file: File): Unit = {
            if (file != null) {
              if (file.isFile) {
                file.delete()
              } else {
                if (file.getAbsolutePath == ve.base.toString) return
                if (file.listFiles.isEmpty) file.delete()
              }
              cleanupUpwards(file.getParentFile)
            }
          }
          cleanupUpwards(ve.uri.toFile)
        })
        super.run()
        timestampPersistFinished = System.nanoTime()
        reportSemanticdbSummary()
      }
    }
  }

  private val timestampPluginCreated = System.nanoTime()
  private var timestampComputeStarted = -1L
  private var timestampComputeFinished = -1L
  private var timestampPersistStarted = -1L
  private var timestampPersistFinished = -1L

  private def reportSemanticdbSummary(): Unit = {
    val createdSemanticdbsMessage = {
      val howMany = g.currentRun.units.length
      val what = if (howMany == 1) "file" else "files"
      var where = scalametaTargetroot.toString
      where = where.stripSuffix("/").stripSuffix("/.")
      where = where + "/META-INF/semanticdb"
      s"Created $howMany semanticdb $what in $where"
    }
    val performanceOverheadMessage = {
      val computeMs = (timestampComputeFinished - timestampComputeStarted) / 1000000
      val persistMs = (timestampPersistFinished - timestampPersistStarted) / 1000000
      val semanticdbMs = computeMs + persistMs
      val totalMs = (timestampPersistFinished - timestampPluginCreated) / 1000000
      val overhead = s"$computeMs+$persistMs=${semanticdbMs}ms performance overhead"
      val pct = Math.floor(1.0 * semanticdbMs / totalMs * 100).toInt
      s"At the cost of $overhead ($pct% of compilation time)"
    }
    if (config.profiling.isConsole) println(createdSemanticdbsMessage)
    if (config.profiling.isConsole) println(performanceOverheadMessage)
  }
}
