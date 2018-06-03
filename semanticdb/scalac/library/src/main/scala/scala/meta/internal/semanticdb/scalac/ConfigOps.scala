package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.io.PathIO
import scala.meta.io._
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.reporters.Reporter
import scala.util.matching.Regex

case class SemanticdbConfig(
    crashes: CrashMode,
    sourceroot: AbsolutePath,
    targetroot: AbsolutePath,
    mode: SemanticdbMode,
    symbols: SymbolMode,
    types: TypeMode,
    profiling: ProfilingMode,
    fileFilter: FileFilter,
    diagnostics: DiagnosticMode,
    synthetics: SyntheticMode) {
  def syntax: String = {
    val p = SemanticdbPlugin.name
    List(
      "crashes" -> crashes.name,
      "sourceroot" -> sourceroot,
      "targetroot" -> targetroot,
      "mode" -> mode.name,
      "types" -> types.name,
      "symbols" -> symbols.name,
      "profiling" -> profiling.name,
      "include" -> fileFilter.include,
      "exclude" -> fileFilter.exclude,
      "diagnostics" -> diagnostics.name,
      "synthetics" -> synthetics.name
    ).map { case (k, v) => s"-P:$p:$k:$v" }.mkString(" ")
  }

}
object SemanticdbConfig {
  def default = SemanticdbConfig(
    CrashMode.Warning,
    PathIO.workingDirectory,
    PathIO.workingDirectory,
    SemanticdbMode.Fat,
    SymbolMode.Definitions,
    TypeMode.All,
    ProfilingMode.Off,
    FileFilter.matchEverything,
    DiagnosticMode.All,
    SyntheticMode.All
  )

  private val SetFailures = "failures:(.*)".r
  private val SetCrashes = "crashes:(.*)".r
  private val SetSourceroot = "sourceroot:(.*)".r
  private val SetMode = "mode:(.*)".r
  private val SetDenotations = "denotations:(.*)".r
  private val SetSymbolInformation = "symbols:(.*)".r
  private val SetSignatures = "signatures:(.*)".r
  private val SetTypes = "types:(.*)".r
  private val SetMembers = "members:(.*)".r
  private val SetOverrides = "overrides:(.*)".r
  private val SetProfiling = "profiling:(.*)".r
  private val SetInclude = "include:(.*)".r
  private val SetExclude = "exclude:(.*)".r
  private val SetMessages = "messages:(.*)".r
  private val SetDiagnostics = "diagnostics:(.*)".r
  private val SetSynthetics = "synthetics:(.*)".r

  def parse(
      scalacOptions: List[String],
      errFn: String => Unit,
      reporter: Reporter
  ): SemanticdbConfig = {
    var config = default
    val relevantOptions = scalacOptions.filter(_.startsWith("-P:semanticdb:"))
    val strippedOptions = relevantOptions.map(_.stripPrefix("-P:semanticdb:"))
    strippedOptions.foreach {
      case option @ SetFailures(CrashMode(crashes)) =>
        reporter.warning(
          NoPosition,
          s"-P:semanticdb:$option is deprecated. " +
            s"Use -P:semanticdb:crashes:{error,warning,info,ignore} instead.")
        config = config.copy(crashes = crashes)
      case SetCrashes(CrashMode(crashes)) =>
        config = config.copy(crashes = crashes)
      case SetSourceroot(path) =>
        config = config.copy(sourceroot = AbsolutePath(path))
      case SetMode(SemanticdbMode(mode)) =>
        config = config.copy(mode = mode)
      case option @ SetDenotations("all") =>
        errFn(s"$option is no longer supported.")
      case option @ SetDenotations(SymbolMode(denotations)) =>
        // TODO(olafur): remove this on next breaking release
        reporter.warning(
          NoPosition,
          s"-P:semanticdb:$option is deprecated. " +
            s"Use -P:semanticdb:symbols:{definitions,all,none} instead.")
        config = config.copy(symbols = denotations)
      case SetSymbolInformation(SymbolMode(infos)) =>
        config = config.copy(symbols = infos)
      case option @ SetSignatures(_) =>
        errFn(
          s"-P:semanticb:$option is no longer supported. " +
            s"Use -P:semanticdb:types:{all,none} instead.")
      case SetTypes(TypeMode(types)) =>
        config = config.copy(types = types)
      case option @ SetMembers(_) =>
        errFn(s"$option is no longer supported.")
      case option @ SetOverrides(_) =>
        errFn(s"$option is no longer supported.")
      case SetProfiling(ProfilingMode(profiling)) =>
        config = config.copy(profiling = profiling)
      case SetInclude(include) =>
        config = config.copy(fileFilter = config.fileFilter.copy(include = include.r))
      case SetExclude(exclude) =>
        config = config.copy(fileFilter = config.fileFilter.copy(exclude = exclude.r))
      case option @ SetMessages(DiagnosticMode(messages)) =>
        reporter.warning(
          NoPosition,
          s"-P:semanticdb:$option is deprecated. " +
            s"Use -P:semanticdb:diagnostics:{all,none} instead.")
        config = config.copy(diagnostics = messages)
      case SetDiagnostics(DiagnosticMode(diagnostics)) =>
        config = config.copy(diagnostics = diagnostics)
      case SetSynthetics(SyntheticMode(synthetics)) =>
        config = config.copy(synthetics = synthetics)
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

sealed abstract class SemanticdbMode {
  def name: String = toString.toLowerCase
  import SemanticdbMode._
  def isSlim: Boolean = this == Slim
  def isFat: Boolean = this == Fat
  def isDisabled: Boolean = this == Disabled
}
object SemanticdbMode {
  def unapply(arg: String): Option[SemanticdbMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Fat, Slim, Disabled)
  case object Fat extends SemanticdbMode
  case object Slim extends SemanticdbMode
  case object Disabled extends SemanticdbMode
}

sealed abstract class SymbolMode {
  def name: String = toString.toLowerCase
  import SymbolMode._
  def isDefinitions: Boolean = this == Definitions
  def isNone: Boolean = this == None
}
object SymbolMode {
  def unapply(arg: String): Option[SymbolMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Definitions, None)
  case object Definitions extends SymbolMode
  case object None extends SymbolMode
}

sealed abstract class TypeMode {
  def name: String = toString.toLowerCase
  import TypeMode._
  def isAll: Boolean = this == All
  def isNone: Boolean = this == None
}
object TypeMode {
  def unapply(arg: String): Option[TypeMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object None extends TypeMode
  case object All extends TypeMode
}

sealed abstract class ProfilingMode {
  def name: String = toString.toLowerCase
  import ProfilingMode._
  def isConsole: Boolean = this == Console
  def isOff: Boolean = this == Off
}
object ProfilingMode {
  def unapply(arg: String): Option[ProfilingMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Console, Off)
  case object Console extends ProfilingMode
  case object Off extends ProfilingMode
}

case class FileFilter(include: Regex, exclude: Regex) {
  def matches(path: String): Boolean =
    include.findFirstIn(path).isDefined &&
      exclude.findFirstIn(path).isEmpty
}
object FileFilter {
  def apply(include: String, exclude: String): FileFilter =
    FileFilter(include.r, exclude.r)
  val matchEverything = FileFilter(".*", "$a")
}

sealed abstract class DiagnosticMode {
  def name: String = toString.toLowerCase
  import DiagnosticMode._
  def saveMessages: Boolean = this == All
}
object DiagnosticMode {
  def unapply(arg: String): Option[DiagnosticMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object All extends DiagnosticMode
  case object None extends DiagnosticMode
}

sealed abstract class SyntheticMode {
  def name: String = toString.toLowerCase
  import SyntheticMode._
  def saveSynthetics: Boolean = this == All
}
object SyntheticMode {
  def unapply(arg: String): Option[SyntheticMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object All extends SyntheticMode
  case object None extends SyntheticMode
}
