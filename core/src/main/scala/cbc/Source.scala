package cbc

sealed trait Source {
  def content: Array[Char]
}

final case class StringSource(val s: String) extends Source {
  def content = s.toArray
}
