package scala.meta.internal
package semanticdb

import scala.meta.internal.io.PathIO
import scala.meta.internal.SemanticdbPlugin
import scala.meta.io._
import scala.util.matching.Regex

case class SemanticdbConfig(
    sourceroot: AbsolutePath,
    mode: SemanticdbMode,
    failures: FailureMode,
    members: MemberMode,
    overrides: OverrideMode,
    denotations: DenotationMode,
    profiling: ProfilingMode,
    fileFilter: FileFilter,
    messages: MessageMode,
    synthetics: SyntheticMode) {
  def syntax: String = {
    val p = SemanticdbPlugin.name
    List(
      "sourceroot" -> sourceroot,
      "mode" -> mode.name,
      "failures" -> failures.name,
      "members" -> members.name,
      "overrides" -> overrides.name,
      "denotations" -> denotations.name,
      "profiling" -> profiling.name,
      "include" -> fileFilter.include,
      "exclude" -> fileFilter.exclude,
      "messages" -> messages.name,
      "synthetics" -> synthetics.name
    ).map { case (k, v) => s"-P:$p:$k:$v" }.mkString(" ")
  }

}
object SemanticdbConfig {
  def default = SemanticdbConfig(
    PathIO.workingDirectory,
    SemanticdbMode.Fat,
    FailureMode.Warning,
    MemberMode.None,
    OverrideMode.None,
    DenotationMode.All,
    ProfilingMode.Off,
    FileFilter.matchEverything,
    MessageMode.All,
    SyntheticMode.All
  )
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

sealed abstract class FailureMode {
  def name: String = toString.toLowerCase
}
object FailureMode {
  def unapply(arg: String): Option[FailureMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Error, Warning, Info, Ignore)
  case object Error extends FailureMode
  case object Warning extends FailureMode
  case object Info extends FailureMode
  case object Ignore extends FailureMode
}

sealed abstract class DenotationMode {
  import DenotationMode._
  def name: String = toString.toLowerCase
  def saveDefinitions: Boolean = this == All || this == Definitions
  def saveReferences: Boolean = this == All
}
object DenotationMode {
  def name: String = toString.toLowerCase
  def unapply(arg: String): Option[DenotationMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, Definitions, None)
  case object All extends DenotationMode
  case object Definitions extends DenotationMode
  case object None extends DenotationMode
}

sealed abstract class MemberMode {
  import MemberMode._
  def name: String = toString.toLowerCase
  def isAll: Boolean = this == All
}
object MemberMode {
  def unapply(arg: String): Option[MemberMode] =
    all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object All extends MemberMode
  case object None extends MemberMode
}

sealed abstract class OverrideMode {
  import OverrideMode._
  def name: String = toString.toLowerCase
  def isAll: Boolean = this == All
}
object OverrideMode {
  def unapply(arg: String): Option[OverrideMode] =
    all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object All extends OverrideMode
  case object None extends OverrideMode
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

sealed abstract class MessageMode {
  def name: String = toString.toLowerCase
  import MessageMode._
  def saveMessages: Boolean = this == All
}
object MessageMode {
  def unapply(arg: String): Option[MessageMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object All extends MessageMode
  case object None extends MessageMode
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

trait ConfigOps { self: DatabaseOps =>
  val SetSourceroot = "sourceroot:(.*)".r
  val SetMode = "mode:(.*)".r
  val SetFailures = "failures:(.*)".r
  val SetMembers = "members:(.*)".r
  val SetDenotations = "denotations:(.*)".r
  val SetOverrides = "overrides:(.*)".r
  val SetProfiling = "profiling:(.*)".r
  val SetInclude = "include:(.*)".r
  val SetExclude = "exclude:(.*)".r
  val SetMessages = "messages:(.*)".r
  val SetSynthetics = "synthetics:(.*)".r

  var config: SemanticdbConfig = SemanticdbConfig.default
  implicit class XtensionSemanticdbConfig(ignored: SemanticdbConfig) {
    def setSourceroot(sourceroot: AbsolutePath): Unit =
      config = config.copy(sourceroot = sourceroot)
    def setMode(mode: SemanticdbMode): Unit =
      config = config.copy(mode = mode)
    def setFailures(severity: FailureMode): Unit =
      config = config.copy(failures = severity)
    def setMembers(members: MemberMode): Unit =
      config = config.copy(members = members)
    def setOverrides(overrides: OverrideMode): Unit =
      config = config.copy(overrides = overrides)
    def setDenotations(denotations: DenotationMode): Unit =
      config = config.copy(denotations = denotations)
    def setProfiling(profiling: ProfilingMode): Unit =
      config = config.copy(profiling = profiling)
    def setInclude(include: String): Unit =
      config = config.copy(fileFilter = config.fileFilter.copy(include = include.r))
    def setExclude(exclude: String): Unit =
      config = config.copy(fileFilter = config.fileFilter.copy(exclude = exclude.r))
    def setMessages(messages: MessageMode): Unit =
      config = config.copy(messages = messages)
    def setSynthetics(synthetics: SyntheticMode): Unit =
      config = config.copy(synthetics = synthetics)
  }
}
