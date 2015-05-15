package scala.meta
package macros

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.meta.macros.{Context => MacroContext}

private[meta] trait Api {
  @hosted(macroApi = true) def resources: Seq[String] = implicitly[MacroContext].resources.keys.toList.sortBy(Predef.identity _)
  @hosted(macroApi = true) def resource(url: String): Array[Byte] = implicitly[MacroContext].resources(url)
}