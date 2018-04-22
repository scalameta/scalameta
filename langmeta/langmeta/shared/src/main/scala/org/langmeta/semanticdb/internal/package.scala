package org.langmeta.internal

import java.nio.file.Files
import java.nio.file.StandardOpenOption
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.io._
import scala.meta.internal.{semanticdb3 => s}

package object semanticdb {
  final val DeprecationMessage =
    "This API is no longer supported. Use scala.meta.internal.semanticdb3._ instead, but note that this API is subject to binary and source breaking changes without notice."
  implicit class XtensionSchemaTextDocument(sdocument: s.TextDocument) {
    private def write(targetroot: AbsolutePath, append: Boolean): Unit = {
      val options =
        if (append) Array(StandardOpenOption.APPEND)
        else Array(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      val out = v.SemanticdbPaths.toSemanticdb(sdocument, targetroot)
      Files.createDirectories(out.toNIO.getParent)
      val fos = Files.newOutputStream(out.toNIO, options: _*)
      try {
        s.TextDocuments(sdocument :: Nil).writeTo(fos)
      } finally {
        fos.close()
      }
    }
    def save(targetroot: AbsolutePath): Unit =
      write(targetroot, append = false)
    def append(targetroot: AbsolutePath): Unit =
      write(targetroot, append = true)
  }

  implicit class XtensionSymbolInformationProperty(info: s.SymbolInformation) {
    def has(prop: s.SymbolInformation.Property): Boolean =
      (info.properties & prop.value) != 0
  }

}
