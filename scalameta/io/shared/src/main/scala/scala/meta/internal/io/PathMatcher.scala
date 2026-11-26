package scala.meta.internal.io

import java.nio.file.Path
import java.util.regex.Pattern

trait PathMatcher {
  def matches(path: Path): Boolean
}

object PathMatcher {

  /** Matches a path if a part of the path's string matches the pattern. */
  class Regex(pattern: Pattern) extends PathMatcher {
    def matches(path: Path): Boolean = pattern.matcher(path.toString).find()
  }

  object Regex {
    def apply(regex: String): Regex =
      try new Regex(Pattern.compile(regex))
      catch {
        // a deeply nested regex overflows the stack
        case e: Throwable =>
          val msg = s"Invalid path patcher regex: /$regex/; ${e.getMessage}"
          throw new InvalidPathPatternException(msg)
      }
  }

  /**
   * Returns the matcher of a pattern in the form `glob:<glob>` or `regex:<regex>`.
   *
   * @throws InvalidPathPatternException
   *   if the pattern is not valid
   */
  def apply(pattern: String): PathMatcher = PlatformPathMatcher(pattern)

  // We need double backslashes here because Regex needs to be escaped.
  def fixSeparatorsInPathPattern(pattern: String): String =
    if (PathIO.isWindows) pattern.replace("/", "\\\\") else pattern

  def inPathMatcherForm(pattern: String): String =
    if (PlatformFlags.isNativeOnWindows) pattern.replace("\\\\", "/")
    else fixSeparatorsInPathPattern(pattern)

}
