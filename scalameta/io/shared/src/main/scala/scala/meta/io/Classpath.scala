package scala.meta.io

import java.io.File.pathSeparator
import java.nio.file.Path

final case class Classpath(entries: List[AbsolutePath]) {

  def ++(other: Classpath): Classpath =
    Classpath(entries ++ other.entries)

  @deprecated("Use .entries instead", "4.0.0")
  def shallow: List[AbsolutePath] = entries

  def syntax: String = entries.mkString(pathSeparator)
  def structure: String = s"""Classpath("$syntax")"""
  override def toString: String = syntax
}

object Classpath {
  def apply(entry: AbsolutePath): Classpath = {
    new Classpath(List(entry))
  }

  def apply(value: String): Classpath = {
    new Classpath(value.split(pathSeparator).map(AbsolutePath(_)).toList)
  }

  def apply(entry: Path): Classpath = {
    Classpath(AbsolutePath(entry))
  }
}
