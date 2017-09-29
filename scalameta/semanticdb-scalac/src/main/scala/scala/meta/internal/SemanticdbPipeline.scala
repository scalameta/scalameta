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

  object ComputeSemanticdbComponent extends PluginComponent {
    val global: SemanticdbPipeline.this.global.type = SemanticdbPipeline.this.global
    val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "semanticdb-compute"
    override val description = "compute semanticdb"
    def newPhase(_prev: Phase) = new ComputeSemanticdbPhase(_prev)
    class ComputeSemanticdbPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        try {
          if (config.mode.isDisabled || !unit.source.file.name.endsWith(".scala")) return
          val fullName = unit.source.file.file.getAbsolutePath
          if (!fullName.matches(config.include) || fullName.matches(config.exclude)) return
          val mattrs = unit.toDocument
          unit.body.updateAttachment(mattrs)
        } catch handleError(unit)
      }

      override def run(): Unit = {
        timestampComputeStarted = System.nanoTime()
        super.run()
        timestampComputeFinished = System.nanoTime()
      }
    }
  }

  object PersistSemanticdbComponent extends PluginComponent {
    val global: SemanticdbPipeline.this.global.type = SemanticdbPipeline.this.global
    val runsAfter = List("jvm")
    override val runsRightAfter = Some("jvm")
    val phaseName = "semanticdb-persist"
    override val description = "persist semanticdb files"
    def newPhase(_prev: Phase) = new PersistSemanticdbPhase(_prev)
    class PersistSemanticdbPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        if (config.mode.isDisabled) return
        try {
          unit.body.attachments.get[m.Document].foreach { mattrs =>
            unit.body.removeAttachment[m.Document]
            val messages = if (config.messages.saveMessages) unit.reportedMessages else Nil
            val mminidb = m.Database(List(mattrs.copy(messages = messages)))
            mminidb.save(scalametaTargetroot, config.sourceroot)
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
