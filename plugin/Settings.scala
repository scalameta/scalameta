package scala.meta
package internal.hosts.scalacompiler

import scala.meta.internal.hosts.scalacompiler.{PluginBase => PalladiumPlugin}

object Settings {
  class Setting[T](get: () => T, set: T => Unit) {
    def value = get()
    def value_=(value: T) = set(value)
  }

  def boolSetting(key: String) = new Setting[Boolean](
    get = () => {
      val svalue = System.getProperty("scalahost." + key)
      svalue != null
    },
    set = value => {
      val svalue = if (value) "true" else null
      System.setProperty("scalahost." + key, svalue)
    }
  )

  def YfictionalLineNumbers = boolSetting("fictional.line.numbers")
  def YshiftingLineNumbers = boolSetting("shifting.line.numbers")
}

trait PluginSettings {
  self: PalladiumPlugin =>

  override def processOptions(options: List[String], error: String => Unit) {
    options foreach {
      case "-Yfictional-line-numbers" => Settings.YfictionalLineNumbers.value = true
      case "-Yshifting-line-numbers" => Settings.YshiftingLineNumbers.value = true
      case option => error("Option not understood: " + option)
    }
  }

  override val optionsHelp: Option[String] = Some("""
    |  -P:scalahost:
    |      -Yfictional-line-numbers     Give macro expansions fictional line numbers (1, 2, 3, 30, 31, 32, 4, ...)
    |      -Yshifting-line-numbers      Give macro expansions shifting line numbers (1, 2, 3, 4, 5, 6, 7, ...)
  """.trim.stripMargin)
}