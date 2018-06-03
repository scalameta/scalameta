package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.io.PathIO
import scala.meta.io._
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.reporters.Reporter
import scala.util.matching.Regex

case class SemanticdbConfig(
    crashes: CrashMode,
    profiling: BinaryMode,
    fileFilter: FileFilter,
    sourceroot: AbsolutePath,
    targetroot: AbsolutePath,
    text: BinaryMode,
    symbols: BinaryMode,
    occurrences: BinaryMode,
    diagnostics: BinaryMode,
    synthetics: BinaryMode) {
  def syntax: String = {
    val p = SemanticdbPlugin.name
    List(
      "crashes" -> crashes.name,
      "profiling" -> profiling.name,
      "include" -> fileFilter.include,
      "exclude" -> fileFilter.exclude,
      "sourceroot" -> sourceroot,
      "targetroot" -> targetroot,
      "text" -> text.name,
      "symbols" -> symbols.name,
      "occurrences" -> occurrences.name,
      "diagnostics" -> diagnostics.name,
      "synthetics" -> synthetics.name
    ).map { case (k, v) => s"-P:$p:$k:$v" }.mkString(" ")
  }

}
object SemanticdbConfig {
  def default = SemanticdbConfig(
    CrashMode.Warning,
    BinaryMode.Off,
    FileFilter.matchEverything,
    PathIO.workingDirectory,
    PathIO.workingDirectory,
    BinaryMode.On,
    BinaryMode.On,
    BinaryMode.On,
    BinaryMode.On,
    BinaryMode.On
  )

  private val SetCrashes = "crashes:(.*)".r
  private val SetProfiling = "profiling:(.*)".r
  private val SetInclude = "include:(.*)".r
  private val SetExclude = "exclude:(.*)".r
  private val SetSourceroot = "sourceroot:(.*)".r
  private val SetText = "text:(.*)".r
  private val SetSymbols = "symbols:(.*)".r
  private val SetOccurrences = "occurrences:(.*)".r
  private val SetDiagnostics = "diagnostics:(.*)".r
  private val SetSynthetics = "synthetics:(.*)".r
  // ============ COMPATIBILITY WITH 3.X STARTS ============
  private val SetMode = "mode:(.*)".r
  private val SetFailures = "failures:(.*)".r
  private val SetOwners = "owners:(.*)".r
  private val SetDenotations = "denotations:(.*)".r
  private val SetSignatures = "signatures:(.*)".r
  private val SetMembers = "members:(.*)".r
  private val SetOverrides = "overrides:(.*)".r
  private val SetMessages = "messages:(.*)".r
  // ============ COMPATIBILITY WITH 3.X ENDS ============

  def parse(
      scalacOptions: List[String],
      errFn: String => Unit,
      reporter: Reporter
  ): SemanticdbConfig = {
    def deprecated(option: String, instead: String): Unit = {
      reporter.warning(
        NoPosition,
        s"-P:semanticdb:$option is deprecated. Use -P:semanticdb:$instead instead.")
    }
    def unsupported(option: String, instead: String = ""): Unit = {
      val buf = new StringBuilder
      buf.append(s"-P:semanticdb:$option is no longer supported.")
      if (instead.nonEmpty) buf.append(s" . Use -P:semanticdb:$instead instead.")
      errFn(buf.toString)
    }
    var config = default
    val relevantOptions = scalacOptions.filter(_.startsWith("-P:semanticdb:"))
    val strippedOptions = relevantOptions.map(_.stripPrefix("-P:semanticdb:"))
    strippedOptions.foreach {
      case SetCrashes(CrashMode(crashes)) =>
        config = config.copy(crashes = crashes)
      case SetProfiling(BinaryMode(mode)) =>
        config = config.copy(profiling = mode)
      case SetInclude(include) =>
        config = config.copy(fileFilter = config.fileFilter.copy(include = include.r))
      case SetExclude(exclude) =>
        config = config.copy(fileFilter = config.fileFilter.copy(exclude = exclude.r))
      case SetSourceroot(path) =>
        config = config.copy(sourceroot = AbsolutePath(path))
      case SetText(BinaryMode(mode)) =>
        config = config.copy(text = mode)
      case SetSymbols(BinaryMode(mode)) =>
        config = config.copy(symbols = mode)
      case SetOccurrences(BinaryMode(mode)) =>
        config = config.copy(occurrences = mode)
      case SetDiagnostics(BinaryMode(mode)) =>
        config = config.copy(diagnostics = mode)
      case SetSynthetics(BinaryMode(mode)) =>
        config = config.copy(synthetics = mode)
      // ============ COMPATIBILITY WITH 3.X STARTS ============
      case option @ SetMode("fat") =>
        deprecated(option, "text:on")
        config = config.copy(text = BinaryMode.On)
      case option @ SetMode("slim") =>
        deprecated(option, "text:off")
        config = config.copy(text = BinaryMode.Off)
      case option @ SetMode("disabled") =>
        unsupported(option, "exclude:^$")
      case option @ SetFailures(CrashMode(crashes)) =>
        deprecated(option, "crashes:{error,warning,info,ignore}")
        config = config.copy(crashes = crashes)
      case option @ SetDenotations(_) =>
        unsupported(option, "symbols")
      case option @ SetSignatures(_) =>
        unsupported(option)
      case option @ SetMembers(_) =>
        unsupported(option)
      case option @ SetOverrides(_) =>
        unsupported(option)
      case option @ SetProfiling("console") =>
        deprecated(option, "profiling:on")
        config = config.copy(profiling = BinaryMode.On)
      case option @ SetMessages("all") =>
        deprecated(option, "diagnostics:on")
        config = config.copy(diagnostics = BinaryMode.On)
      case option @ SetMessages("none") =>
        deprecated(option, "diagnostics:off")
        config = config.copy(diagnostics = BinaryMode.Off)
      case option @ SetSynthetics("all") =>
        deprecated(option, "synthetics:on")
        config = config.copy(synthetics = BinaryMode.On)
      case option @ SetSynthetics("none") =>
        deprecated(option, "synthetics:off")
        config = config.copy(synthetics = BinaryMode.Off)
      case option @ SetOwners(_) =>
        unsupported(option)
      // ============ COMPATIBILITY WITH 3.X ENDS ============
      case els =>
        errFn(s"Ignoring unknown option $els")
    }
    config
  }
}

sealed abstract class CrashMode {
  def name: String = toString.toLowerCase
}
object CrashMode {
  def unapply(arg: String): Option[CrashMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Error, Warning, Info, Ignore)
  case object Error extends CrashMode
  case object Warning extends CrashMode
  case object Info extends CrashMode
  case object Ignore extends CrashMode
}

sealed abstract class BinaryMode {
  def name: String = toString.toLowerCase
  import BinaryMode._
  def isOn: Boolean = this == On
  def isOff: Boolean = this == Off
}
object BinaryMode {
  def unapply(arg: String): Option[BinaryMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(On, Off)
  case object On extends BinaryMode
  case object Off extends BinaryMode
}

case class FileFilter(include: Regex, exclude: Regex) {
  def matches(path: String): Boolean =
    include.findFirstIn(path).isDefined &&
      exclude.findFirstIn(path).isEmpty
}
object FileFilter {
  def apply(include: String, exclude: String): FileFilter =
    FileFilter(include.r, exclude.r)
  val matchEverything = FileFilter(".*", "$^")
}
