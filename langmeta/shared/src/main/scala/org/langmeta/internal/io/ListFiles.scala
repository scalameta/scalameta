package org.langmeta.internal.io

import org.langmeta.io.AbsolutePath
import org.langmeta.io.RelativePath

final case class ListFiles(root: AbsolutePath, files: List[RelativePath]) extends Seq[AbsolutePath] {
  override def length: Int = files.length
  override def apply(idx: Int): AbsolutePath = root.resolve(files.apply(idx))
  override def iterator: Iterator[AbsolutePath] = files.iterator.map(root.resolve)
}
