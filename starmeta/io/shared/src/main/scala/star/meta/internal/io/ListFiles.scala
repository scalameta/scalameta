package star.meta.internal.io

import star.meta.io.AbsolutePath
import star.meta.io.RelativePath

final case class ListFiles(root: AbsolutePath, files: List[RelativePath]) extends Seq[AbsolutePath] {
  override def length: Int = files.length
  override def apply(idx: Int): AbsolutePath = root.resolve(files.apply(idx))
  override def iterator: Iterator[AbsolutePath] = files.iterator.map(root.resolve)
}
