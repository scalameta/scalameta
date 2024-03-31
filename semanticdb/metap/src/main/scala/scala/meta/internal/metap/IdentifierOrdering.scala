package scala.meta.internal.metap

/**
 * A comparator for identifier like "Predef" or "Function10".
 *
 * Differences from the default string comparator:
 *   - works with CharSequences like compiler `Name`
 *   - orders numbers by their numerical value instead of lexicographical
 *     - Good: `Function1`, `Function2`, `Function10`
 *     - Bad: `Function1`, `Function10`, `Function2`
 */
class IdentifierOrdering[T <: CharSequence] extends Ordering[T] {
  override def compare(o1: T, o2: T): Int = {
    val len = math.min(o1.length(), o2.length())
    var i = 0
    while (i < len) {
      val a = o1.charAt(i)
      val b = o2.charAt(i)
      if (a.isDigit && b.isDigit) {
        val byDigit = Integer.compare(toDigit(o1, i), toDigit(o2, i))
        if (byDigit != 0) return byDigit else i = seekNonDigit(o1, i)
      } else {
        val result = Character.compare(a, b)
        if (result != 0) return result
        i += 1
      }
    }
    Integer.compare(o1.length(), o2.length())
  }
  private def seekNonDigit(cs: T, i: Int): Int = {
    var curr = i
    while (curr < cs.length() && cs.charAt(curr).isDigit) curr += 1
    curr
  }
  private def toDigit(cs: T, i: Int): Int = {
    val digit = cs.subSequence(i, seekNonDigit(cs, i))
    Integer.parseUnsignedInt(digit.toString)
  }
}
