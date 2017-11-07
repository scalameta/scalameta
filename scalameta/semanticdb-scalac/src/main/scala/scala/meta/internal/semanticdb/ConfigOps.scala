package scala.meta.internal
package semanticdb

import scala.meta.internal.io.PathIO
import scala.meta.internal.SemanticdbPlugin
import scala.meta.io._

case class SemanticdbConfig(
    sourceroot: AbsolutePath,
    mode: SemanticdbMode,
    failures: FailureMode,
    denotations: DenotationMode,
    profiling: ProfilingMode,
    include: String,
    exclude: String,
    messages: MessageMode,
    synthetics: SyntheticMode) {
  def syntax: String =
    s"-P:${SemanticdbPlugin.name}:sourceroot:$sourceroot " +
      s"-P:${SemanticdbPlugin.name}:mode:${mode.name}" +
      s"-P:${SemanticdbPlugin.name}:failures:${failures.name} " +
      s"-P:${SemanticdbPlugin.name}:denotations:${denotations.name} " +
      s"-P:${SemanticdbPlugin.name}:profiling:${profiling.name} " +
      s"-P:${SemanticdbPlugin.name}:include:$include " +
      s"-P:${SemanticdbPlugin.name}:exclude:$exclude " +
      s"-P:${SemanticdbPlugin.name}:messages:${messages.name} " +
      s"-P:${SemanticdbPlugin.name}:synthetics:${synthetics.name} "
}
object SemanticdbConfig {
  def default =
    SemanticdbConfig(
      PathIO.workingDirectory,
      SemanticdbMode.Fat,
      FailureMode.Warning,
      DenotationMode.All,
      ProfilingMode.Off,
      ".*",
      "",
      MessageMode.All,
      SyntheticMode.All)
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
  def name: String = toString.toLowerCase
  import DenotationMode._
  def saveDefinitions: Boolean = this == All || this == Definitions
  def saveReferences: Boolean = this == All
}
object DenotationMode {
  def unapply(arg: String): Option[DenotationMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, Definitions, None)
  case object All extends DenotationMode
  case object Definitions extends DenotationMode
  case object None extends DenotationMode
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
  val SetDenotations = "denotations:(.*)".r
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
    def setDenotations(denotations: DenotationMode): Unit =
      config = config.copy(denotations = denotations)
    def setProfiling(profiling: ProfilingMode): Unit =
      config = config.copy(profiling = profiling)
    def setInclude(include: String): Unit =
      config = config.copy(include = include)
    def setExclude(exclude: String): Unit =
      config = config.copy(exclude = exclude)
    def setMessages(messages: MessageMode): Unit =
      config = config.copy(messages = messages)
    def setSynthetics(synthetics: SyntheticMode): Unit =
      config = config.copy(synthetics = synthetics)
  }
}
