package scala.meta.internal.semanticdb.scalac

import java.io._
import java.net.URI
import scala.compat.Platform.EOL
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.util.control.NonFatal
import scala.meta.internal.{semanticdb3 => s}

trait SemanticdbPipeline extends SemanticdbOps { self: SemanticdbPlugin =>
  implicit class XtensionURI(uri: URI) { def toFile: File = new File(uri) }
  implicit class XtensionUnit(unit: g.CompilationUnit) {
    def isIgnored: Boolean = {
      config.mode.isDisabled || {
        !unit.source.file.name.endsWith(".scala") && !unit.source.file.name.endsWith(".sc")
      } || {
        Option(unit.source.file)
          .flatMap(f => Option(f.file))
          .map(_.getAbsolutePath)
          .exists(
            fullName => !config.fileFilter.matches(fullName)
          )
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
      import scala.meta.internal.semanticdb.scalac.FailureMode._
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
    override val description = "compute and persist SemanticDB after typer"
    def newPhase(_prev: Phase) = new ComputeSemanticdbPhase(_prev)
    class ComputeSemanticdbPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        try {
          if (unit.isIgnored) return
          validateCompilerState()
          val sdoc = unit.toTextDocument
          sdoc.save(config.targetroot)
        } catch handleError(unit)
      }

      private def synchronizeSourcesAndSemanticdbFiles(): Unit = {
        RemoveOrphanSemanticdbFiles.process(config.sourceroot, config.targetroot)
      }

      private def synchronizeSourcesAndSemanticdbIndex(): Unit = {
        // TODO: Support incremental compilation.
        index.save(config.targetroot)
      }

      override def run(): Unit = {
        timestampComputeStarted = System.nanoTime()
        super.run()
        synchronizeSourcesAndSemanticdbFiles()
        synchronizeSourcesAndSemanticdbIndex()
        timestampComputeFinished = System.nanoTime()
        idCache.clear()
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
      "compute and persist additional SemanticDB messages created after typer"
    def newPhase(_prev: Phase) = new PersistSemanticdbPhase(_prev)
    class PersistSemanticdbPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        if (unit.isIgnored) return
        try {
          if (config.diagnostics.saveMessages) {
            val diagnostics = unit.reportedDiagnostics(Map.empty)
            if (diagnostics.nonEmpty) {
              val sdoc = s.TextDocument(
                schema = s.Schema.SEMANTICDB3,
                uri = unit.source.toUri,
                language = s.Language.SCALA,
                diagnostics = diagnostics
              )
              sdoc.append(config.targetroot)
            }
          }
        } catch handleError(unit)
      }

      override def run(): Unit = {
        timestampPersistStarted = System.nanoTime()
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
      var where = config.targetroot.toString
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
