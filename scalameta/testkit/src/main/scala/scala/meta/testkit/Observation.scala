package scala.meta.testkit

import scala.collection.mutable

/** An observation during a [[SyntaxAnalysis]].
  *
  * @param msg The message corresponding this observation.
  * @param line The offending line number in the source file where the
  *             observation was made.
  * @param kind The category of this observation. Good values are enumerations
  *             or sealed ADTs
  */
case class Observation[T](msg: String, line: Int, kind: T)

object Observation {
  private def wrapInCode(msg: String): String =
    s"<code>${msg.replaceAll("\n", "</br>")}</code>"

  /** Returns a markdown table displaying all observations, publish-ready for Github.
    *
    * Example: https://github.com/scalameta/scalameta/issues/567#issuecomment-267074738
    */
  def markdownTable[T](
      observations: Seq[(CorpusFile, Observation[T])]): String = {
    val sb = new mutable.StringBuilder()
    val grouped = observations.groupBy(_._2.kind)
    grouped.foreach {
      case (cat, rs) =>
        sb.append(s"## $cat\n")
        sb.append(s"URL | details |\n")
        sb.append(s"--- | --- |\n")
        rs.sortBy(_._2.msg).foreach {
          case (f, x) =>
            sb.append(s"${f.githubUrlAtLine(x.line)} | ${wrapInCode(x.msg)}\n")
        }
    }
    grouped.foreach {
      case (cat, rs) =>
        sb.append(s"$cat: ${rs.length}\n")
    }
    sb.toString()
  }
}