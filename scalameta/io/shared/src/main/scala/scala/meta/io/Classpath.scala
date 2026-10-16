package scala.meta.io

import java.io.File.pathSeparator
import java.nio.file.Path

final case class Classpath(entries: List[AbsolutePath]) {

  def ++(other: Classpath): Classpath = Classpath(entries ++ other.entries)

  /** Keep only the entries matching the predicate. */
  def filter(f: AbsolutePath => Boolean): Classpath = Classpath(entries.filter(f))

  @deprecated("Use .entries instead", "4.0.0")
  private[meta] def shallow: List[AbsolutePath] = entries

  def syntax: String = entries.mkString(pathSeparator)
  def structure: String = s"""Classpath("$syntax")"""
  override def toString: String = syntax
}

object Classpath {
  def apply(entry: AbsolutePath): Classpath = new Classpath(List(entry))

  private[meta] def toPaths(value: String) = value.split(pathSeparator).toList
  private[meta] def fromPaths(paths: List[String]) = new Classpath(paths.map(AbsolutePath(_)))

  def apply(value: String): Classpath = fromPaths(toPaths(value))
  def apply(value: String*): Classpath = fromPaths(value.flatMap(toPaths).toList)

  def apply(entry: Path): Classpath = Classpath(AbsolutePath(entry))
}
