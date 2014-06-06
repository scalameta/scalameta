package scala.reflect
package syntactic

import scala.language.existentials // SI-6541

package object parsers {
  private[reflect] type Offset = Int
  private[reflect] type Token = Int

  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )

  // Shorten a name like Symbols$FooSymbol to FooSymbol.
  private def shortenName(name: String): String = {
    if (name == "") return ""
    val segments = (name split '$').toList
    val last     = segments.last

    if (last.length == 0)
      segments takeRight 2 mkString "$"
    else
      last
  }

  def shortClassOfInstance(x: AnyRef): String = shortClass(x.getClass)
  def shortClass(clazz: Class[_]): String = {
    val name: String = (clazz.getName split '.').last
    def isModule     = name endsWith "$"                        // object
    def isAnon       = (name split '$').last forall (_.isDigit) // anonymous class

    if (isModule)
      (name split '$' filterNot (_ == "")).last + "$"
    else if (isAnon)
      clazz.getSuperclass :: clazz.getInterfaces.toList map (c => shortClass(c)) mkString " with "
    else
      shortenName(name)
  }

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
