package scala.meta.internal.io

import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath

final case class ListFiles(root: AbsolutePath, files: List[RelativePath]) extends Seq[AbsolutePath] {
  override def length: Int = files.length
  override def apply(idx: Int): AbsolutePath = root.resolve(files.apply(idx))
  override def iterator: Iterator[AbsolutePath] = files.iterator.map(root.resolve)
}
