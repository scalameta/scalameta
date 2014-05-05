/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect.parser.util

import scala.compat.Platform.EOL

/** This object provides utility methods to extract elements
 *  from Strings.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object StringOps {
  def longestCommonPrefix(xs: List[String]): String = xs match {
    case Nil      => ""
    case w :: Nil => w
    case _        =>
      def lcp(ss: List[String]): String = {
        val w :: ws = ss
        if (w == "") ""
        else if (ws exists (s => s == "" || (s charAt 0) != (w charAt 0))) ""
        else w.substring(0, 1) + lcp(ss map (_ substring 1))
      }
      lcp(xs)
  }
  def splitWhere(str: String, f: Char => Boolean, doDropIndex: Boolean = false): Option[(String, String)] =
    splitAt(str, str indexWhere f, doDropIndex)
  def splitAt(str: String, idx: Int, doDropIndex: Boolean = false): Option[(String, String)] =
    if (idx == -1) None
    else Some((str take idx, str drop (if (doDropIndex) idx + 1 else idx)))
}
