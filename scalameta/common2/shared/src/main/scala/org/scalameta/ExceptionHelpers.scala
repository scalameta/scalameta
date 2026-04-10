package org.scalameta

import org.scalameta.internal.ScalaCompat.EOL

private[scalameta] object ExceptionHelpers {
  def formatDebuggees(sb: StringBuilder, debuggees: Map[String, Any]): Unit = debuggees.toList
    .sortBy(_._1).foreach { case (k, v) =>
      val vstr = v.toString
      val vformatted = if (vstr.isEmpty) s"''" else vstr
      sb.append(s"where $k = $vformatted$EOL")
    }
}
