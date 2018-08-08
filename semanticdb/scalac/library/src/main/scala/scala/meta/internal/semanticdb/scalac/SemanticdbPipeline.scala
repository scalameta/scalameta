package scala.meta.internal.semanticdb.scalac

import java.io._
import java.net.URI
import scala.compat.Platform.EOL
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.meta.internal.{semanticdb => s}
import scala.util.control.NonFatal

trait SemanticdbPipeline extends SemanticdbOps { self: SemanticdbPlugin =>
  implicit class XtensionURI(uri: URI) { def toFile: File = new File(uri) }
  implicit class XtensionUnit(unit: g.CompilationUnit) {
    def isIgnored: Boolean = {
      val matchesExtension = {
        val fileName = unit.source.file.name
        fileName.endsWith(".scala") ||
        fileName.endsWith(".sc") ||
        fileName.endsWith(".java")
      }
      val matchesFilter = {
        Option(unit.source.file)
          .flatMap(f => Option(f.file))
          .map(f => config.fileFilter.matches(f.getAbsolutePath))
          .getOrElse(true)
      }
      !matchesExtension || !matchesFilter
    }
  }

  def handleCrash(unit: Option[g.CompilationUnit]): PartialFunction[Throwable, Unit] = {
    case NonFatal(ex) =>
      val writer = new StringWriter()
      val culprit = unit.map(unit => " for " + unit.source.file.path).getOrElse("")
      writer.write(s"failed to generate semanticdb$culprit:$EOL")
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

      def saveSemanticdbForCompilationUnit(unit: g.CompilationUnit): Unit = {
        try {
          if (unit.isIgnored) return
          val sdoc = unit.toTextDocument
          sdoc.save(config.targetroot)
        } catch handleCrash(Some(unit))
      }

      override def apply(unit: g.CompilationUnit): Unit = {
        saveSemanticdbForCompilationUnit(unit)
      }

      private def synchronizeSourcesAndSemanticdbFiles(): Unit = {
        RemoveOrphanSemanticdbFiles.process(config)
      }

      override def run(): Unit = {
        try {
          timestampComputeStarted = System.nanoTime()
          super.run()
          synchronizeSourcesAndSemanticdbFiles()
          timestampComputeFinished = System.nanoTime()
          idCache.clear()
          symbolCache.clear()
          gSourceFileInputCache.clear()
        } catch handleCrash(None)
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
          if (config.diagnostics.isOn) {
            val diagnostics = unit.reportedDiagnostics(Map.empty)
            if (diagnostics.nonEmpty) {
              val sdoc = s.TextDocument(
                schema = s.Schema.SEMANTICDB4,
                uri = unit.source.toUri,
                language = s.Language.SCALA,
                diagnostics = diagnostics
              )
              sdoc.append(config.targetroot)
            }
          }
        } catch handleCrash(Some(unit))
      }

      override def run(): Unit = {
        try {
          timestampPersistStarted = System.nanoTime()
          super.run()
          timestampPersistFinished = System.nanoTime()
          reportSemanticdbSummary()
        } catch handleCrash(None)
      }
    }
  }

  private val timestampPluginCreated = System.nanoTime()
  private var timestampComputeStarted = -1L
  private var timestampComputeFinished = -1L
  private var timestampPersistStarted = -1L
  private var timestampPersistFinished = -1L

  private def reportSemanticdbSummary(): Unit = {
    def createdSemanticdbsMessage = {
      val howMany = g.currentRun.units.length
      val what = if (howMany == 1) "file" else "files"
      var where = config.targetroot.toString
      where = where.stripSuffix("/").stripSuffix("/.")
      where = where + "/META-INF/semanticdb"
      s"Created $howMany semanticdb $what in $where"
    }
    def performanceOverheadMessage = {
      val computeMs = (timestampComputeFinished - timestampComputeStarted) / 1000000
      val persistMs = (timestampPersistFinished - timestampPersistStarted) / 1000000
      val semanticdbMs = computeMs + persistMs
      val totalMs = (timestampPersistFinished - timestampPluginCreated) / 1000000
      val overhead = s"$computeMs+$persistMs=${semanticdbMs}ms performance overhead"
      val pct = Math.floor(1.0 * semanticdbMs / totalMs * 100).toInt
      s"At the cost of $overhead ($pct% of compilation time)"
    }
    if (config.profiling.isOn) {
      println(createdSemanticdbsMessage)
      println(performanceOverheadMessage)
    }
  }
}
