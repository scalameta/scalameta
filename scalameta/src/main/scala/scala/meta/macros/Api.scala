package scala.meta
package macros

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.meta.macros.{Context => MacroContext}

private[meta] trait Api {
  @hosted(macroApi = true) def warning(msg: String): Unit = implicitly[MacroContext].warning(msg)
  @hosted(macroApi = true) def error(msg: String): Unit = implicitly[MacroContext].error(msg)
  @hosted(macroApi = true) def abort(msg: String): Nothing = implicitly[MacroContext].abort(msg)
  @hosted(macroApi = true) def resources: Seq[String] = implicitly[MacroContext].resources.keys.toList.sortBy(Predef.identity _)
  @hosted(macroApi = true) def resource(url: String): Array[Byte] = implicitly[MacroContext].resources(url)
}