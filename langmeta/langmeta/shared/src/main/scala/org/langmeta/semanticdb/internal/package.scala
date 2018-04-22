package org.langmeta.internal

import java.io.File
import java.io.FileOutputStream
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.io._
import scala.meta.internal.{semanticdb3 => s}

package object semanticdb {
  final val DeprecationMessage =
    "This API is no longer supported. Use scala.meta.internal.semanticdb3._ instead, but note that this API is subject to binary and source breaking changes without notice."
  implicit class XtensionSchemaTextDocuments(sdocuments: s.TextDocuments) {
    def toVfs(targetroot: AbsolutePath): v.Database = {
      val ventries = sdocuments.documents.toIterator.map { sentry =>
        // TODO: Would it make sense to support multiclasspaths?
        // One use-case for this would be in-place updates of semanticdb files.
        val vpath = v.SemanticdbPaths.fromScala(RelativePath(sentry.uri))
        val fragment = Fragment(targetroot, vpath)
        val bytes = s.TextDocuments(List(sentry)).toByteArray
        v.Entry.InMemory(fragment, bytes)
      }
      v.Database(ventries.toList)
    }
  }

  implicit class XtensionSymbolInformationProperty(info: s.SymbolInformation) {
    def has(prop: s.SymbolInformation.Property): Boolean =
      (info.properties & prop.value) != 0
  }

}
